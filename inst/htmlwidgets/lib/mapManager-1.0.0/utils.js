function newLayer(manager, x, initial_visible) {
  if (x.type == "weight") {
    var y = new WeightLayer(
      manager,
      x.id,
      x.name,
      initial_visible,
      x.units,
      x.legend
    );
  } else {
    if (typeof(x.feature_name) === "string") {
      var y = undefined;
      // TODO: new SingleThemeLayer(...)
    } else {
      var y = undefined;
      // TODO: new MultiThemeLayer(...)
    }
  }
  return y;
}
