function newWeightSetting(manager, x) {
  return new WeightSetting(
    manager,
    x.id,
    x.name,
    x.min_factor,
    x.max_factor,
    x.factor,
    x.step_factor,
    x.status
  );
}

function newIncludeSetting(manager, x) {
  return new IncludeSetting(
    manager,
    x.id,
    x.name,
    x.status,
    x.mandatory
  );
}

function newParameterSetting(manager, x) {
  return new ParameterSetting(
    manager,
    x.id,
    x.name,
    x.min_value,
    x.min_value,
    x.value,
    x.step_value,
    x.status,
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
      x.feature_total_amount,
      x.feature_current_held,
      x.feature_min_goal,
      x.feature_max_goal,
      x.feature_goal,
      x.feature_limit_goal,
      x.feature_step_goal,
      x.feature_status,
      x.units,
      x.mandatory,
      x.icon
    );
  } else {
    var y = new MultiThemeSetting(
      manager,
      x.id,
      x.name,
      x.feature_name,
      x.feature_id,
      x.feature_total_amount,
      x.feature_current_held,
      x.feature_min_goal,
      x.feature_max_goal,
      x.feature_goal,
      x.feature_limit_goal,
      x.feature_step_goal,
      x.feature_status,
      x.feature_icon,
      x.units,
      x.mandatory,
      x.icon
    );
  }
  return y;
}
