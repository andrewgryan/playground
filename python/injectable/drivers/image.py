from bokeh.plotting import Figure
import bokeh.models


def dataset(driver):
    """Public interface"""
    source = bokeh.models.ColumnDataSource(
        data=dict(x=[], y=[], dw=[], dh=[], image=[])
    )
    color_mappers = []

    def update(model):
        image = driver(2 ** model.resolution)
        source.data = {
            "x": [0],
            "y": [0],
            "dw": [1e6],
            "dh": [1e6],
            "image": [image],
        }
        if model.palette is not None:
            for color_mapper in color_mappers:
                color_mapper.palette = model.palette

    def add_figure(figure):
        glyph_renderer = figure.image(
            x="x", y="y", dw="dw", dh="dh", image="image", source=source
        )
        color_mappers.append(glyph_renderer.glyph.color_mapper)

        def show(visible):
            glyph_renderer.visible = visible

        return show

    return update, add_figure


def driver(resolution: int):
    """Generate image data at particular resolution"""
    image = []
    for i in range(resolution):
        row = []
        for j in range(resolution):
            row.append(i + j)
        image.append(row)
    return image
