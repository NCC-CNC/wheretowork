function newLayer(manager, x) {
  if (x.type == "solution") {
    var y = new SolutionLayer(
      manager,
      x.id,
      x.name,
      x.statistics,
      x.visible,
      x.legend,
      x.units
    );
  } else if (x.type == "include") {
    var y = new IncludeLayer(
      manager,
      x.id,
      x.name,
      x.visible,
      x.legend,
      x.units
    );
  } else if (x.type == "weight") {
    var y = new WeightLayer(
      manager,
      x.id,
      x.name,
      x.visible,
      x.legend,
      x.units
    );
  } else {
    if (typeof(x.feature_name) === "string") {
      var y = new SingleThemeLayer(
        manager,
        x.id,
        x.name,
        x.feature_id,
        x.feature_name,
        x.feature_visible,
        x.feature_legend,
        x.units
      );
    } else {
      var y = new MultiThemeLayer(
        manager,
        x.id,
        x.name,
        x.feature_id,
        x.feature_name,
        x.feature_visible,
        x.feature_legend,
        x.units
      );
    }
  }
  return y;
}
