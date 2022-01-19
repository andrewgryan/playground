import xarray
import bokeh.models


def dataset(file_name: str):
    """Collection of data"""

    ds = xarray.open_dataset(file_name)

    # Bokeh wiring
    source = bokeh.models.ColumnDataSource(
        data=dict(x=[], y=[], dw=[], dh=[], image=[])
    )
    color_mappers = []

    def update(model):
        """React to model changes"""
        print(f"{file_name}: {model.variable}")
        if model.variable is not None:
            if model.variable in ds.data_vars:
                data_array = ds.data_vars[model.variable]
                print(data_array)
                if data_array.ndim == 3:
                    image = data_array[0].to_numpy()
                    source.data = {
                        "x": [0],
                        "y": [0],
                        "dw": [1e6],
                        "dh": [1e6],
                        "image": [image],
                    }
                elif data_array.ndim == 4:
                    image = data_array[0, 0].to_numpy()
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
        """Register graphical representation with figure"""
        glyph_renderer = figure.image(
            x="x", y="y", dw="dw", dh="dh", image="image", source=source
        )
        color_mappers.append(glyph_renderer.glyph.color_mapper)

        def show(visible: bool):
            """Allow user to toggle visibility"""
            glyph_renderer.visible = visible

        return show

    return update, add_figure
