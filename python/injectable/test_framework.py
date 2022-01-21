from framework import update, Model, Dataset, Layer, OnSelected


def test_update():
    model = Model(datasets=[Dataset(0, "foo", ["bar"])])
    msgs = [
        OnSelected("left", "dataset", [], ["foo"]),
        OnSelected("left", "variable", [], ["bar"]),
    ]
    for msg in msgs:
        model = update(model, msg)
    actual = model.layers
    expected = set([Layer(0, 0, "bar")])
    assert actual == expected


def test_update_remove_layer():
    model = Model(datasets=[Dataset(0, "foo", ["bar"])])
    msgs = [
        OnSelected("left", "dataset", [], ["foo"]),
        OnSelected("left", "variable", [], ["bar"]),
        OnSelected("left", "variable", ["bar"], []),
    ]
    for msg in msgs:
        model = update(model, msg)
    actual = model.layers
    expected = set()
    assert actual == expected


def test_update_different_figures():
    model = Model(datasets=[Dataset(0, "foo", ["bar"])])
    msgs = [
        OnSelected("right", "dataset", [], ["foo"]),
        OnSelected("right", "variable", [], ["bar"]),
    ]
    for msg in msgs:
        model = update(model, msg)
    actual = model.layers
    expected = set([Layer(1, 0, "bar")])
    assert actual == expected


def test_update_incompatible_figures():
    model = Model(datasets=[Dataset(0, "foo", ["bar"])])
    msgs = [
        OnSelected("right", "dataset", [], ["foo"]),
        OnSelected("left", "variable", [], ["bar"]),
    ]
    for msg in msgs:
        model = update(model, msg)
    actual = model.layers
    expected = set()
    assert actual == expected
