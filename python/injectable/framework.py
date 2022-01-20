import yaml
from typing import List, Callable, Optional, Dict
from dataclasses import dataclass, replace, field
import bokeh.plotting
from bokeh.plotting import Figure
from bokeh.tile_providers import CARTODBPOSITRON, get_provider
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
class SetVariables:
    label: str
    variables: Dict[str, List[str]]


@dataclass
class SetVariable:
    variable: str


@dataclass
class SetLayer:
    side: str
    variables: List[str]


Msg = SubOne | AddOne | NoOp | HideShow | TapMap | SetVariables | SetVariable | SetLayer


# MODEL


@dataclass
class Model:
    resolution: int = 1
    palette: List[str] = field(default_factory=lambda: bokeh.palettes.cividis(256))
    visible: List[bool] = field(default_factory=lambda: [True, True])
    point: Optional[Point] = None
    variables: Dict[str, List[str]] = field(default_factory=dict)
    variable: Optional[str] = None
    left: List[str] = field(default_factory=list)
    right: List[str] = field(default_factory=list)


def init() -> Model:
    """Initialize model"""
    return Model()


View = Callable[[Figure], Callable[[Model, bool], None]]


def attach_layers(figures, viewers):
    """Wire up row of figures to drivers/views"""

    layers = [
        (update, [add_figure(figure) for figure in figures])
        for update, add_figure in viewers
    ]

    def inner(model):
        """React to model changes"""
        for update, row in layers:
            update(model)
            for visible, show in zip(model.visible, row):
                show(visible)

    return inner


def attach_point(figures):
    sources = []
    for figure in figures:
        source = bokeh.models.ColumnDataSource(data={"x": [], "y": []})
        figure.circle(x="x", y="y", size=5, source=source)
        sources.append(source)

    def inner(point: Optional[Point]):
        if point is not None:
            data = {"x": [point.x], "y": [point.y]}
            for source in sources:
                source.data = data

    return inner


def attach_profile(figure):
    source = bokeh.models.ColumnDataSource(data={"x": [], "y": []})
    figure.circle(x="x", y="y", size=5, color="red", source=source)
    figure.line(x="x", y="y", color="red", source=source)

    def inner(point: Optional[Point]):
        if point is not None:
            data = {"x": [point.x], "y": [point.y]}
            source.stream(data)

    return inner


def attach_series(figure):
    import datetime

    source = bokeh.models.ColumnDataSource(data={"x": [], "y": []})
    figure.circle(x="x", y="y", size=5, color="red", source=source)
    figure.line(x="x", y="y", color="red", source=source)

    def inner(point: Optional[Point]):
        if point is not None:
            data = {"x": [datetime.datetime.now()], "y": [point.y]}
            source.stream(data)

    return inner


def app(document, send_msg):
    """Application"""

    # Dataset/Driver API
    datasets = []
    config = Config(**yaml.safe_load(open("config.yaml")))
    for file_name in config.file_names:
        datasets.append(niwa.dataset(file_name))

    # Maps on figure row
    map_figures = [map_figure(send_msg, title="Left"), map_figure(send_msg, title="Right")]

    # Placeholder for profile/time series
    series_figure = bokeh.plotting.figure(
        title="Time series",
        x_axis_type="datetime",
        y_axis_type="mercator",
        margin=(4,4,4,4),
        css_classes=["rounded", "shadow"]
    )
    profile_figure = bokeh.plotting.figure(
        title="Vertical profile",
        x_axis_type="mercator",
        y_axis_type="mercator",
        margin=(4,4,4,4),
        css_classes=["rounded", "shadow"]
    )

    # Bokeh document
    ui_nav, render_nav = navigation(send_msg)
    document.add_root(ui_nav)
    document.add_root(
        bokeh.layouts.column(
            bokeh.layouts.column(
                bokeh.layouts.row(*map_figures, spacing=4, sizing_mode="stretch_both"),
                bokeh.layouts.row(
                    series_figure, profile_figure, spacing=4, sizing_mode="stretch_both"
                ),
                spacing=4,
            ),
            sizing_mode="stretch_both",
        )
    )

    # Create renderers
    render_layers = attach_layers(map_figures, datasets)
    render_point = attach_point(map_figures)
    render_profile = attach_profile(profile_figure)
    render_series = attach_series(series_figure)

    def inner(model):
        render_layers(model)
        render_point(model.point)
        render_profile(model.point)
        render_series(model.point)
        render_nav(model)

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
    for file_name in config.file_names:
        thread = Thread(target=get_variables, args=(runner.send, file_name))
        thread.start()


def runtime(document):
    """Continually process msg and model updates"""
    model = init()
    render = yield
    render(model)
    msg = yield
    while True:
        print(msg)
        model = update(model, msg)
        document.add_next_tick_callback(partial(render, model))
        msg = yield


def get_variables(send_msg, file_name):
    import xarray

    # Perform I/O
    ds = xarray.open_dataset(file_name)
    variables = sorted(ds.data_vars)

    # Send data to application
    send_msg(SetVariables(file_name, variables))


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
        case SetVariables(label, variables):
            model.variables[label] = variables  # Note: in-place modification
            return model
        case SetVariable(variable):
            return replace(model, variable=variable)
        case SetLayer(side, variables):
            if side == "left":
                return replace(model, left=variables)
            elif side == "right":
                return replace(model, right=variables)
        case NoOp():
            return model


def navigation(send_msg):
    select = bokeh.models.Select()

    def on_change(attr, old, new):
        send_msg(SetVariable(new))

    select.on_change("value", on_change)

    # Multi-select visibility
    title_left = bokeh.models.Div(text="Left choices", css_classes=["text-lg", "py-2"])
    multi_left = bokeh.models.MultiChoice()
    multi_left_vars = bokeh.models.MultiChoice()

    multi_right = bokeh.models.MultiChoice()
    multi_right_vars = bokeh.models.MultiChoice()

    def on_multi(side):
        def wrapper(attr, old, new):
            send_msg(SetLayer(side, new))

        return wrapper

    title_right = bokeh.models.Div(text="Right choices", css_classes=["text-lg", "py-2"])
    multi_right.on_change("value", on_multi("right"))
    multi_right_vars.on_change("value", on_multi("right"))
    multi_left.on_change("value", on_multi("left"))
    multi_left_vars.on_change("value", on_multi("left"))

    def render(model):
        select.disabled = len(model.variables) == 0
        select.options = model.variables
        if model.variable is not None:
            if model.variable != select.value:
                select.value = model.variable

        # Dataset names
        if model.variables is not None:
            vars = []
            for items in model.variables.values():
                vars += items
            vars = sorted(set(vars))

            multi_left.options = list(model.variables.keys())
            multi_right.options = list(model.variables.keys())
            multi_left_vars.options = vars
            multi_right_vars.options = vars

    return (
        bokeh.layouts.column(
            select,
            title_left,
            multi_left,
            multi_left_vars,
            title_right,
            multi_right,
            multi_right_vars,
            name="navigation",
        ),
        render,
    )


def map_figure(send_msg, **figure_kwargs) -> bokeh.plotting.Figure:
    figure = bokeh.plotting.figure(
        x_range=(-2e6, 2e6),
        y_range=(-2e6, 2e6),
        x_axis_type="mercator",
        y_axis_type="mercator",
        margin=(4,4,4,4),
        css_classes=["rounded", "shadow"],
        **figure_kwargs
    )
    provider = get_provider(CARTODBPOSITRON)
    figure.add_tile(provider)

    # Add Tap event
    def callback(event):
        send_msg(TapMap(Point(event.x, event.y)))

    figure.on_event(Tap, callback)

    return figure
