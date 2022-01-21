import xarray
import bokeh.models


class Profile:
    """Visualise profile related to layer at point"""

    def __init__(self, ds, figure, layer):
        self.ds = ds
        self.figure = figure
        self.layer = layer
        self.glyph_renderers = []
        self.source = None

    def render(self, point):
        if self.source is None:
            self.source = bokeh.models.ColumnDataSource({"x": [], "y": []})
            self.glyph_renderers = [
                self.figure.line(x="x", y="y", source=self.source),
                self.figure.circle(x="x", y="y", source=self.source),
            ]
        else:
            self.show(True)
        if self.layer.variable in self.ds:
            data_array = self.ds[self.layer.variable]
            if data_array.ndim == 4:
                print(data_array)
                for dim in data_array.coords:
                    if dim.startswith("pressure"):
                        pressure_dim = dim
                        break
                x = data_array[0, :, 0, 0].values
                y = data_array[pressure_dim].values
                print(x, y)
                self.source.data = {"x": x, "y": y}

    def remove(self):
        self.show(False)
        self.source.data = {"x": [], "y": []}

    def show(self, visible: bool):
        for glyph_renderer in self.glyph_renderers:
            glyph_renderer.visible = visible


class Dataset:
    def __init__(self, file_name: str):
        self.file_name = file_name
        self.ds = xarray.open_dataset(self.file_name)

    def profile(self, figure, layer):
        """Support time series functionality"""
        return Profile(self.ds, figure, layer)

    def time_series(self):
        """Support time series functionality"""
        if "mslp" in self.ds:
            data_array = self.ds["mslp"]
            time_dim = "time"
            for dim in data_array.coords:
                if dim.startswith("time"):
                    time_dim = dim
                    break
            ts = data_array[:, 0, 0]
            x = ts.coords[time_dim].values
            y = ts.values
        else:
            x = []
            y = []

        def plotter(figure):
            """Register figure"""
            source = bokeh.models.ColumnDataSource({"x": x, "y": y})
            glyph_renderer = figure.line(x="x", y="y", source=source)

            def render(point):
                """Render point"""
                source.data = {"x": [point.x], "y": [point.y]}

            return render

        return plotter

    def map_layer(self):
        """Collection of data"""

        # Functions to hide/show layers
        registry = {}
        color_mappers = []

        def update(source, variable, palette):
            """React to model changes"""
            if variable is not None:
                if variable in self.ds.data_vars:
                    data_array = self.ds.data_vars[variable]
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
