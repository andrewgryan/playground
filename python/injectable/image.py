from bokeh.plotting import Figure
import bokeh.models


def viewer(figure):
    """Public interface"""
    view = _view(figure)

    def inner(model, visible):
        data = _driver(2 ** model.resolution)
        view(data, model.palette, visible)

    return inner


def _driver(resolution: int):
    """Generate image data at particular resolution"""
    image = []
    for i in range(resolution):
        row = []
        for j in range(resolution):
            row.append(i + j)
        image.append(row)
    return image


def _view(figure: Figure):
    source = bokeh.models.ColumnDataSource(
        data=dict(x=[], y=[], dw=[], dh=[], image=[])
    )

    glyph_renderer = figure.image(
        x="x", y="y", dw="dw", dh="dh", image="image", source=source
    )
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
            "image": [image],
        }

    return view
