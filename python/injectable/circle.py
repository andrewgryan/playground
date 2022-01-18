import math
import bokeh.models
from bokeh.plotting import Figure


def viewer(figure):
    """Public interface"""
    view = _view(figure)

    def inner(model, visible):
        data = _driver(model.resolution)
        view(data, visible)

    return inner


def _driver(resolution: int):
    """Private driver"""
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
    return {"x": xs, "y": ys}


def _view(figure: Figure):
    """Private view"""
    source = bokeh.models.ColumnDataSource(
        data=dict(x=[], y=[], dw=[], dh=[], image=[])
    )

    glyph_renderer = figure.line(x="x", y="y", source=source)

    def view(data, visible):
        """Called on every model update"""
        glyph_renderer.visible = visible
        source.data = data

    return view
