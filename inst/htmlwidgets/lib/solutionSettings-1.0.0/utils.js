function newWeightSetting(manager, x) {
  return new WeightSetting(
    manager,
    x.id,
    x.name,
    x.min_factor,
    x.max_factor,
    x.factor,
    x.step_factor,
    x.status,
    x.provenance
  );
}

function newIncludeSetting(manager, x) {
  return new IncludeSetting(
    manager,
    x.id,
    x.name,
    x.status,
    x.overlap,
    x.mandatory,
    x.provenance
  );
}

function newExcludeSetting(manager, x) {
  return new ExcludeSetting(
    manager,
    x.id,
    x.name,
    x.status,
    x.overlap,
    x.mandatory,
    x.provenance
  );
}

function newParameterSetting(manager, x) {
  return new ParameterSetting(
    manager,
    x.id,
    x.name,
    x.status,
    x.value,
    x.min_value,
    x.max_value,
    x.step_value,
    x.hide,
    x.disable,
    x.fileinput,
    x.units,
    x.reference_value,
    x.reference_units,
    x.tool_tip
  );
}

function newThemeSetting(manager, x) {
  if (typeof(x.feature_name) === "string") {
    var y = new SingleThemeSetting(
      manager,
      x.id,
      x.name,
      x.feature_name,
      x.feature_id,
      x.feature_status,
      x.feature_total_amount,
      x.feature_current_held,
      x.feature_min_goal,
      x.feature_max_goal,
      x.feature_goal,
      x.feature_limit_goal,
      x.feature_step_goal,
      x.feature_provenance[0],
      x.units || "units"
    );
  } else {
    var y = new MultiThemeSetting(
      manager,
      x.id,
      x.name,
      x.feature_name,
      x.feature_id,
      x.feature_status,
      x.feature_total_amount,
      x.feature_current_held,
      x.feature_min_goal,
      x.feature_max_goal,
      x.feature_goal,
      x.feature_limit_goal,
      x.feature_step_goal,
      x.feature_provenance,
      x.units || "units"
    );
  }
  return y;
}
