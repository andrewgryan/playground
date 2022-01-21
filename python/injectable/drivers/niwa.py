import xarray
import bokeh.models


def dataset(file_name: str):
    """Collection of data"""

    ds = xarray.open_dataset(file_name)

    # Functions to hide/show layers
    registry = {}
    color_mappers = []

    def update(source, variable, palette):
        """React to model changes"""
        print(f"{file_name}: {variable}")
        if variable is not None:
            if variable in ds.data_vars:
                data_array = ds.data_vars[variable]
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
        if palette is not None:
            for color_mapper in color_mappers:
                color_mapper.palette = palette

    def add_figure(figure):
        """Register graphical representation with figure"""
        source = bokeh.models.ColumnDataSource(
            data=dict(x=[], y=[], dw=[], dh=[], image=[])
        )
        glyph_renderer = figure.image(
            x="x", y="y", dw="dw", dh="dh", image="image", source=source
        )
        color_mappers.append(glyph_renderer.glyph.color_mapper)

        def show(visible: bool):
            """Allow user to toggle visibility"""
            glyph_renderer.visible = visible

        return show, source

    # return update, add_figure
    def add_layer(figure, model, variable):
        key = (figure.id, variable)
        if key in registry:
            show, _ = registry[key]
            show(True)
        else:
            show, source = add_figure(figure)
            update(source, variable, model.palette)
            registry[key] = (show, source)

    def remove_layer(figure, variable):
        key = (figure.id, variable)
        show, _ = registry[key]
        show(False)

    return add_layer, remove_layer
