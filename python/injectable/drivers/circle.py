import math
import bokeh.models
from bokeh.plotting import Figure


def dataset(driver):
    """Public interface"""
    source = bokeh.models.ColumnDataSource(
        data=dict(x=[], y=[], dw=[], dh=[], image=[])
    )

    def update(model):
        print(model.variable)
        source.data = driver(model.resolution)

    def add_figure(figure):
        glyph_renderer = figure.line(x="x", y="y", source=source)

        def show(visible):
            glyph_renderer.visible = visible

        return show

    return update, add_figure


def driver(resolution: int):
    """Public driver"""
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
