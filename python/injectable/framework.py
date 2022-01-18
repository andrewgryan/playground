from typing import List, Callable
from dataclasses import dataclass, replace
import bokeh.plotting
from bokeh.plotting import Figure
from bokeh.tile_providers import CARTODBPOSITRON, get_provider
import bokeh.palettes


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
    return [[viewer(figure) for figure in figures] for viewer in viewers]


def render_layers(layers, model):
    for row in layers:
        for visible, view in zip(model.visible, row):
            view(model, visible)


def map_row(figures, views):
    """High-level controller"""
    layers = attach_layers(figures, views)

    def inner(model):
        render_layers(layers, model)

    return inner


def run():
    """Entry point"""

    # Configure view(s)
    roots = [map_root(), map_root()]
    view = map_row(roots, [image_viewer, circle_viewer])

    # Elm architecture
    runner = runtime(view)
    runner.send(None)

    # Bokeh document
    bokeh.plotting.curdoc().add_root(
            bokeh.layouts.column(
                control(runner),
                bokeh.layouts.row(*roots),
                ))

def circle_viewer(figure):
    view = circle_view(figure)
    def inner(model, visible):
        data = circle_driver(model.resolution)
        view(data, visible)

    return inner

def circle_driver(resolution: int):
    import math
    xs = []
    ys = []
    radius = 2e6
    n = 2 ** resolution
    for i in range(n + 1):
        angle = (i / float(n)) * (2 * math.pi)
        x = radius * math.sin(angle)
        y = radius * math.cos(angle)
        xs.append(x)
        ys.append(y)
    return {
        "x": xs,
        "y": ys
    }


def circle_view(figure: Figure):
    source = bokeh.models.ColumnDataSource(data=dict(
        x=[],
        y=[],
        dw=[],
        dh=[],
        image=[]))

    glyph_renderer = figure.line(
            x="x",
            y="y",
            source=source)

    def view(data, visible):
        """Called on every model update"""
        glyph_renderer.visible = visible
        source.data = data

    return view


def runtime(render):
    """Continually process msg and model updates"""
    model = init()
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


def image_viewer(figure):
    view = image_view(figure)

    def inner(model, visible):
        data = image_driver(2 ** model.resolution)
        view(data, model.palette, visible)

    return inner


def image_driver(resolution: int):
    """Generate image data at particular resolution"""
    image = []
    for i in range(resolution):
        row = []
        for j in range(resolution):
            row.append(i + j)
        image.append(row)
    return image

def image_view(figure: Figure):
    source = bokeh.models.ColumnDataSource(data=dict(
        x=[],
        y=[],
        dw=[],
        dh=[],
        image=[]))

    glyph_renderer = figure.image(
            x="x",
            y="y",
            dw="dw",
            dh="dh",
            image="image",
            source=source)
    color_mapper = glyph_renderer.glyph.color_mapper

    def view(image, palette, visible):
        """Called on every model update"""
        color_mapper.palette = palette
        glyph_renderer.visible = visible
        source.data = {
            "x": [0],
            "y": [0],
            "dw": [1e6],
            "dh": [1e6],
            "image": [image]
        }

    return view


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
