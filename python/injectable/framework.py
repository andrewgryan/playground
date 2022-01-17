from typing import List
from dataclasses import dataclass
import bokeh.plotting
from bokeh.plotting import Figure
from bokeh.tile_providers import CARTODBPOSITRON, get_provider
import bokeh.palettes


@dataclass
class AddOne:
    pass

@dataclass
class SubOne:
    pass

@dataclass
class NoOp:
    pass


Msg = SubOne | AddOne | NoOp


@dataclass
class Model:
    resolution: int
    palette: List[str]


def run():
    root = map_root()
    view = map_view(root, image_driver)

    runner = runtime(view)
    runner.send(None)

    bokeh.plotting.curdoc().add_root(
            bokeh.layouts.column(
                control(runner),
                root,
                ))


def image_driver(resolution: int):
    """Generate image data at particular resolution"""
    image = []
    for i in range(resolution):
        row = []
        for j in range(resolution):
            row.append(i + j)
        image.append(row)
    return image


def runtime(render):
    model = init()
    msg = yield
    while True:
        model = update(model, msg)
        render(model)
        msg = yield


def init() -> Model:
    return Model(4, bokeh.palettes.cividis(256))


def update(model, msg) -> Model:
    match msg:
        case AddOne():
            return Model(model.resolution + 1, model.palette)
        case SubOne():
            return Model(model.resolution - 1, model.palette)
        case NoOp():
            return model


def control(runner):
    btns = {
        "+": bokeh.models.Button(label="+"),
        "-": bokeh.models.Button(label="-")
    }
    btns["+"].on_click(lambda: runner.send(AddOne()))
    btns["-"].on_click(lambda: runner.send(SubOne()))
    return bokeh.layouts.row(btns["-"], btns["+"])


def map_view(figure: Figure, image_driver):
    source = bokeh.models.ColumnDataSource(data=dict(
        x=[0],
        y=[0],
        dw=[1e6],
        dh=[1e6],
        image=[[[1,2,3],[4,5,6],[7,8,9]]]))

    glyph_renderer = figure.image(
            x="x",
            y="y",
            dw="dw",
            dh="dh",
            image="image",
            source=source)
    color_mapper = glyph_renderer.glyph.color_mapper

    def view(model):
        """Called on every model update"""
        color_mapper.palette = model.palette
        if model.resolution > 0:
            image = image_driver(2 ** model.resolution)
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
class MapView:
    pass

@dataclass
class Driver:
    setting: Setting
    map_view: MapView

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


def add_figure(map_view: MapView) -> Renderable:
    pass
