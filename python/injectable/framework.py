import yaml
from typing import List, Callable, Optional, Dict, Set
from dataclasses import dataclass, replace, field
import bokeh.plotting
from bokeh.plotting import Figure
from bokeh.tile_providers import CARTODBPOSITRON, get_provider
from bokeh.models import MercatorAxis
import bokeh.palettes
from bokeh.events import Tap
from drivers import circle, image, niwa
from bokeh.document import without_document_lock
from threading import Thread
import threading
import time
from queue import Queue
from tornado import gen
from functools import partial

# MODEL


@dataclass
class Dataset:
    id: int
    label: str
    variables: List[str] = field(default_factory=list)


@dataclass
class SelectedDataset:
    figure_id: int
    dataset_id: int


@dataclass(eq=True, frozen=True)
class Layer:
    figure_id: int
    dataset_id: int
    variable: str


# MSG


@dataclass
class AddOne:
    pass


@dataclass
class SubOne:
    pass


@dataclass
class HideShow:
    pass


@dataclass
class NoOp:
    pass


@dataclass
class Point:
    x: float
    y: float


@dataclass
class TapMap:
    point: Point


@dataclass
class AddDataset:
    dataset: Dataset


@dataclass
class AddLayer:
    layer: Layer


@dataclass
class OnSelected:
    side: str
    category: str
    old: List[str]
    new: List[str]


Msg = SubOne | AddOne | NoOp | HideShow | TapMap | AddDataset | AddLayer | OnSelected


# MODEL


@dataclass
class Model:
    resolution: int = 1
    palette: List[str] = field(default_factory=lambda: bokeh.palettes.cividis(256))
    visible: List[bool] = field(default_factory=lambda: [True, True])
    point: Optional[Point] = None
    datasets: List[Dataset] = field(default_factory=list)
    layers: Set[Layer] = field(default_factory=set)
    selected_datasets: List[int] = field(default_factory=list)


def init() -> Model:
    """Initialize model"""
    return Model()


View = Callable[[Figure], Callable[[Model, bool], None]]


def attach_layers(figures, datasets):
    """Wire up row of figures to drivers/views"""
    attached = set()

    def inner(model):
        """React to model changes"""
        for layer in attached - model.layers:
            figure = figures[layer.figure_id]
            _, remove_layer = datasets[layer.dataset_id]
            remove_layer(figure, layer.variable)
            attached.remove(layer)

        for layer in model.layers - attached:
            figure = figures[layer.figure_id]
            add_layer, _ = datasets[layer.dataset_id]
            add_layer(figure, model, layer.variable)
            attached.add(layer)

    return inner


def layer_manager():
    """Orchestrate layers"""
    attached = set()

    def removed(layers: Set[Layer]):
        for layer in attached - layers:
            yield layer
            attached.remove(layer)

    def added(layers: Set[Layer]):
        for layer in layers - attached:
            yield layer
            attached.add(layer)

    return removed, added


def attach_point(figures):
    sources = []
    for figure in figures:
        source = bokeh.models.ColumnDataSource(data={"x": [], "y": []})
        figure.circle(
            x="x",
            y="y",
            size=6,
            source=source,
            level="overlay",
            fill_color="white",
            line_color="black",
        )
        sources.append(source)

    def inner(point: Optional[Point]):
        if point is not None:
            data = {"x": [point.x], "y": [point.y]}
            for source in sources:
                source.data = data

    return inner


def attach_profile(figure, datasets):
    """Orchestrate add/remove profile layers"""
    registry = {}

    def to_key(layer):
        """Unique data structure to cache profile renderers"""
        return layer.dataset_id, layer.variable

    def add(layer: Layer, point: Point):
        """Call profile renderer methods"""
        key = to_key(layer)
        if key not in registry:
            dataset = datasets[layer.dataset_id]
            registry[key] = dataset.profile(figure, layer)
        registry[key].render(point)

    def remove(layer: Layer):
        """Call profile renderer methods"""
        key = to_key(layer)
        if key in registry:
            registry[key].remove()

    return add, remove


def attach_series(figure, plotters):
    renderers = [plotter(figure) for plotter in plotters]

    def inner(point: Optional[Point]):
        if point is not None:
            for renderer in renderers:
                renderer(point)

    return inner


def app(document, send_msg):
    """Application"""

    # Dataset/Driver API
    datasets = []
    config = Config(**yaml.safe_load(open("config.yaml")))
    for file_name in config.file_names:
        datasets.append(niwa.Dataset(file_name))

    # Maps on figure row
    map_figures = [
        map_figure(send_msg, title="Left"),
        map_figure(send_msg, title="Right"),
    ]

    # Placeholder for profile/time series
    series_figure = bokeh.plotting.figure(
        title="Time series",
        toolbar_location="above",
        x_axis_type="datetime",
        margin=(4, 4, 4, 4),
    )
    profile_figure = bokeh.plotting.figure(
        title="Vertical profile",
        toolbar_location="right",
        margin=(4, 4, 4, 4),
    )
    profile_figure.title.align = "center"

    # Bokeh document
    ui_nav_left, render_nav_left = navigation(send_msg, "left", "Left options")
    ui_nav_right, render_nav_right = navigation(send_msg, "right", "Right options")
    document.add_root(
        bokeh.layouts.column(ui_nav_left, ui_nav_right, name="navigation")
    )
    document.add_root(
        bokeh.layouts.column(
            map_figures[0], sizing_mode="stretch_both", name="map_left"
        )
    )
    document.add_root(
        bokeh.layouts.column(
            map_figures[1], sizing_mode="stretch_both", name="map_right"
        )
    )
    document.add_root(
        bokeh.layouts.column(series_figure, sizing_mode="stretch_both", name="series")
    )
    document.add_root(
        bokeh.layouts.column(profile_figure, sizing_mode="stretch_both", name="profile")
    )

    # Create renderers
    removed, added = layer_manager()
    render_layers = attach_layers(map_figures, [ds.map_layer() for ds in datasets])
    render_point = attach_point(map_figures)
    render_series = attach_series(series_figure, [ds.time_series() for ds in datasets])
    add_profile, remove_profile = attach_profile(profile_figure, datasets)

    def inner(model):
        # Remove
        for layer in removed(model.layers):
            if model.point is not None:
                remove_profile(layer)

        # Add
        for layer in added(model.layers):
            if model.point is not None:
                add_profile(layer, model.point)

        render_layers(model)
        render_point(model.point)
        render_series(model.point)
        render_nav_left(model)
        render_nav_right(model)

    return inner


@dataclass
class Config:
    file_names: List[str]


def run():
    """Entry point"""
    config = Config(**yaml.safe_load(open("config.yaml")))

    # Bokeh document lock across threads
    document = bokeh.plotting.curdoc()

    # Elm architecture
    runner = runtime(document)
    runner.send(None)
    view = app(document, runner.send)
    runner.send(gen.coroutine(view))

    # Simulate I/O
    for i, file_name in enumerate(config.file_names):
        thread = Thread(target=get_variables, args=(runner.send, file_name, i))
        thread.start()


def runtime(document):
    """Continually process msg and model updates"""
    model = init()
    render = yield
    render(model)
    msg = yield
    while True:
        model = update(model, msg)
        document.add_next_tick_callback(partial(render, model))
        msg = yield


def get_variables(send_msg, file_name, dataset_id):
    import xarray

    # Perform I/O
    ds = xarray.open_dataset(file_name)
    variables = sorted(ds.data_vars)

    # Send data to application
    send_msg(AddDataset(Dataset(dataset_id, file_name, variables)))


# UPDATE


def update(model, msg: Msg) -> Model:
    """Update model given message"""
    match msg:
        case AddOne():
            return replace(model, resolution=model.resolution + 1)
        case SubOne():
            return replace(model, resolution=model.resolution - 1)
        case HideShow():
            return replace(model, visible=[not visible for visible in model.visible])
        case TapMap(point):
            return replace(model, point=point)
        case AddDataset(dataset):
            model.datasets.append(dataset)  # Note: in-place modification
            return model
        case AddLayer(layer):
            model.layers.add(layer)  # Note: in-place modification
            return model
        case OnSelected(side, category, old, new):
            figure_id = ["left", "right"].index(side)
            if category == "dataset":
                for label in new:
                    if label not in old:
                        for dataset in model.datasets:
                            if dataset.label == label:
                                model.selected_datasets.append(
                                    SelectedDataset(figure_id, dataset.id)
                                )
            elif category == "variable":
                for selected_dataset in model.selected_datasets:
                    if selected_dataset.figure_id != figure_id:
                        continue
                    for variable in old:
                        layer = Layer(figure_id, selected_dataset.dataset_id, variable)
                        if layer in model.layers:
                            model.layers.remove(layer)
                    for variable in new:
                        layer = Layer(figure_id, selected_dataset.dataset_id, variable)
                        model.layers.add(layer)

            return model
        case NoOp():
            return model


def navigation(send_msg, side, title):

    # Multi-select visibility
    title = bokeh.models.Div(text=title, css_classes=["text-lg", "py-2"])
    multi = bokeh.models.MultiChoice()
    multi_vars = bokeh.models.MultiChoice()

    def on_multi(category):
        def wrapper(attr, old, new):
            send_msg(OnSelected(side, category, old, new))

        return wrapper

    multi.on_change("value", on_multi("dataset"))
    multi_vars.on_change("value", on_multi("variable"))

    def render(model):
        # Dataset names
        if model.datasets is not None:
            vars = []
            for dataset in model.datasets:
                vars += dataset.variables
            vars = sorted(set(vars))

            multi.options = [dataset.label for dataset in model.datasets]
            multi_vars.options = vars

    return (
        bokeh.layouts.column(
            title,
            multi,
            multi_vars,
        ),
        render,
    )


def map_figure(send_msg, **figure_kwargs) -> bokeh.plotting.Figure:
    figure = bokeh.plotting.figure(
        toolbar_location="above",
        x_range=(-2e6, 2e6),
        y_range=(-2e6, 2e6),
        x_axis_type="mercator",
        y_axis_type="mercator",
        margin=(4, 4, 4, 4),
        active_scroll="wheel_zoom",
        **figure_kwargs,
    )

    figure.extra_x_ranges.update({"x_above": figure.x_range})
    figure.add_layout(MercatorAxis("lon", x_range_name="x_above"), "above")

    figure.extra_y_ranges.update({"y_right": figure.y_range})
    figure.add_layout(MercatorAxis("lat", y_range_name="y_right"), "right")

    provider = get_provider(CARTODBPOSITRON)
    figure.add_tile(provider)

    # Add Tap event
    def callback(event):
        send_msg(TapMap(Point(event.x, event.y)))

    figure.on_event(Tap, callback)

    return figure
