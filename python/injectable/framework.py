from typing import List, Callable
from dataclasses import dataclass, replace
import bokeh.plotting
from bokeh.plotting import Figure
from bokeh.tile_providers import CARTODBPOSITRON, get_provider
import bokeh.palettes
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


Msg = SubOne | AddOne | NoOp | HideShow


# MODEL

@dataclass
class Model:
    resolution: int
    palette: List[str]
    visible: List[bool]


def init() -> Model:
    """Initialize model"""
    return Model(1, bokeh.palettes.cividis(256), [True, True])


View = Callable[[Figure], Callable[[Model, bool], None]]


def attach_layers(figures, viewers):
    """Wire up row of figures to drivers/views"""
    return [[viewer(figure) for figure in figures] for viewer in viewers]


def render_layers(layers, model):
    """React to model changes"""
    for row in layers:
        for visible, view in zip(model.visible, row):
            view(model, visible)


def map_row(figures, views):
    """High-level controller"""
    layers = attach_layers(figures, views)

    def inner(model):
        render_layers(layers, model)

    return inner


def app(runner, datasets):
    """Application"""

    # Maps on figure row
    roots = [map_root(), map_root()]

    # Placeholder for profile/time series
    figure = bokeh.plotting.figure()

    # Bokeh document
    bokeh.plotting.curdoc().add_root(
            bokeh.layouts.column(
                control(runner),
                bokeh.layouts.row(*roots),
                figure,
                ))

    return map_row(roots, datasets)



def run():
    """Entry point"""

    # Elm architecture
    runner = runtime()
    runner.send(None)

    # FOREST architecture
    datasets = [image.dataset(image.driver), circle.dataset(circle.driver)]
    view = app(runner, datasets)
    runner.send(view)


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


def map_root() -> bokeh.plotting.Figure:
    figure = bokeh.plotting.figure(
            x_range=(-2e6, 2e6),
            y_range=(-2e6, 2e6),
            x_axis_type="mercator",
            y_axis_type="mercator",
            )
    provider = get_provider(CARTODBPOSITRON)
    figure.add_tile(provider)
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
class Point:
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
