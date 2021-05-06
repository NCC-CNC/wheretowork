function newWeightSetting(manager, x) {
  return new WeightSetting(
    manager,
    x.id,
    x.name,
    x.min_factor,
    x.max_factor,
    x.initial_factor,
    x.step_factor,
    x.initial_status
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
      x.feature_initial_goal,
      x.feature_limit_goal,
      x.feature_step_goal,
      x.feature_current_label,
      x.units,
      x.mandatory,
      x.initial_status,
      x.round,
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
      x.group_min_goal,
      x.group_max_goal,
      x.group_initial_goal,
      x.group_limit_goal,
      x.group_step_goal,
      x.group_current_label,
      x.feature_min_goal,
      x.feature_max_goal,
      x.feature_initial_goal,
      x.feature_limit_goal,
      x.feature_step_goal,
      x.feature_current_label,
      x.feature_initial_status,
      x.feature_icon,
      x.units,
      x.mandatory,
      x.initial_status,
      x.round,
      x.icon
    );
  }
  return y;
}
