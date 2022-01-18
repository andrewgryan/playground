from typing import List, Callable, Optional
from dataclasses import dataclass, replace, field
import bokeh.plotting
from bokeh.plotting import Figure
from bokeh.tile_providers import CARTODBPOSITRON, get_provider
import bokeh.palettes
from bokeh.events import Tap
import circle
import image


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


Msg = SubOne | AddOne | NoOp | HideShow | TapMap


# MODEL

@dataclass
class Model:
    resolution: int = 1
    palette: List[str] = field(default_factory=lambda: bokeh.palettes.cividis(256))
    visible: List[bool] = field(default_factory=lambda: [True, True])
    point: Optional[Point] = None


def init() -> Model:
    """Initialize model"""
    return Model()


View = Callable[[Figure], Callable[[Model, bool], None]]


def attach_layers(figures, viewers):
    """Wire up row of figures to drivers/views"""
    return [[viewer(figure) for figure in figures] for viewer in viewers]


def render_layers(layers, model):
    """React to model changes"""
    for row in layers:
        for visible, view in zip(model.visible, row):
            view(model, visible)

def attach_point(figures):
    sources = []
    for figure in figures:
        source = bokeh.models.ColumnDataSource(data={"x":[], "y": []})
        figure.circle(x="x", y="y", size=5, source=source)
        sources.append(source)

    def inner(point: Optional[Point]):
        if point is not None:
            data = {"x": [point.x], "y": [point.y]}
            for source in sources:
                source.data = data
    return inner


def attach_profile(figure):
    source = bokeh.models.ColumnDataSource(data={"x":[], "y": []})
    figure.circle(x="x", y="y", size=5, color="red", source=source)
    figure.line(x="x", y="y", color="red", source=source)

    def inner(point: Optional[Point]):
        if point is not None:
            data = {"x": [point.x], "y": [point.y]}
            source.stream(data)
    return inner


def attach_series(figure):
    import datetime

    source = bokeh.models.ColumnDataSource(data={"x":[], "y": []})
    figure.circle(x="x", y="y", size=5, color="red", source=source)
    figure.line(x="x", y="y", color="red", source=source)

    def inner(point: Optional[Point]):
        if point is not None:
            data = {"x": [datetime.datetime.now()], "y": [point.y]}
            source.stream(data)
    return inner


def app(runner):
    """Application"""

    datasets = [image.dataset(image.driver), circle.dataset(circle.driver)]

    # Maps on figure row
    map_figures = [map_figure(runner), map_figure(runner)]


    # Placeholder for profile/time series
    series_figure = bokeh.plotting.figure(
            x_axis_type="datetime",
            y_axis_type="mercator",
            )
    profile_figure = bokeh.plotting.figure(
            x_axis_type="mercator",
            y_axis_type="mercator",
            )

    # Bokeh document
    bokeh.plotting.curdoc().add_root(
            bokeh.layouts.column(
                control(runner),
                bokeh.layouts.row(*map_figures, series_figure, profile_figure, sizing_mode="scale_width"),
                sizing_mode="scale_width"))


    layers = attach_layers(map_figures, datasets)
    render_point = attach_point(map_figures)
    render_profile = attach_profile(profile_figure)
    render_series = attach_series(series_figure)

    def inner(model):
        render_layers(layers, model)
        render_point(model.point)
        render_profile(model.point)
        render_series(model.point)

    return inner


def run():
    """Entry point"""

    # Elm architecture
    runner = runtime()
    runner.send(None)
    runner.send(app(runner))


def runtime():
    """Continually process msg and model updates"""
    model = init()
    render = yield
    render(model)
    msg = yield
    while True:
        model = update(model, msg)
        render(model)
        msg = yield


def update(model, msg) -> Model:
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
        case NoOp():
            return model


def control(runner):
    btns = {
        "+": bokeh.models.Button(label="+"),
        "-": bokeh.models.Button(label="-"),
        "h/s": bokeh.models.Button(label="h/s")
    }
    btns["+"].on_click(lambda: runner.send(AddOne()))
    btns["-"].on_click(lambda: runner.send(SubOne()))
    btns["h/s"].on_click(lambda: runner.send(HideShow()))
    return bokeh.layouts.row(btns["-"], btns["+"], btns["h/s"])


def map_figure(runner) -> bokeh.plotting.Figure:
    figure = bokeh.plotting.figure(
            x_range=(-2e6, 2e6),
            y_range=(-2e6, 2e6),
            x_axis_type="mercator",
            y_axis_type="mercator",
            )
    provider = get_provider(CARTODBPOSITRON)
    figure.add_tile(provider)

    # Add Tap event
    def callback(event):
        runner.send(TapMap(Point(event.x, event.y)))

    figure.on_event(Tap, callback)

    return figure

@dataclass
class Setting:
    pass


@dataclass
class Driver:
    setting: Setting

@dataclass
class Renderable:
    pass

@dataclass
class Navigator:
    pass


@dataclass
class Interval:
    pass


@dataclass
class Dimension:
    length: int
    start: Point
    interval: Interval


def driver(setting: Setting) -> Driver:
    return Driver(setting, MapView())


def navigator(driver: Driver) -> Navigator:
    pass


def dimensions(navigator: Navigator) -> List[Dimension]:
    return []
