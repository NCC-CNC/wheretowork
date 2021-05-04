function newWeightFactor(manager, x) {
  return new Weight(
    manager = manager,
    id = x.id,
    name = x.name,
    min_factor = x.min_factor,
    max_factor = x.max_factor,
    factor = x.factor,
    step_factor = x.step_factor,
    status = x.status);
}

function newThemeGoal(manager, x) {
  let x = undefined;
  if (x.feature_name.length === 1) {
    x = new SingleThemeGoal(
      manager = manager,
      id = x.id,
      name = x.name,
      feature_name = x.feature_name,
      feature_id = x.feature_id,
      feature_total_amount = x.feature_total_amount,
      feature_min_goal = x.feature_min_goal,
      feature_max_goal = x.feature_max_goal,
      feature_initial_goal = x.feature_initial_goal,
      feature_step_goal = x.feature_step_goal,
      feature_limit_goal = x.feature_limit_goal,
      feature_current_label = x.feature_current_label,
      feature_current_held = x.feature_current_held,
      feature_units = x.feature_units,
      round = x.round,
      initial_status = x.initial_status,
      icon = x.icon);
  } else {
    x = new MultiThemeGoal(
      manager = manager,
      id = x.id,
      name = x.name,
      feature_name = x.feature_name,
      feature_id = x.feature_id,
      feature_total_amount = x.feature_total_amount,
      feature_current_held = x.feature_current_held,
      group_min_goal = x.group_min_goal,
      group_max_goal = x.group_max_goal,
      group_initial_goal = x.group_initial_goal,
      group_limit_goal = x.group_limit_goal,
      group_step_goal = x.group_step_goal,
      group_current_label = x.group_current_label,
      feature_min_goal = x.feature_min_goal,
      feature_max_goal = x.feature_max_goal,
      feature_initial_goal = x.feature_initial_goal,
      feature_limit_goal = x.feature_limit_goal,
      feature_step_goal = x.feature_step_goal,
      feature_current_label = x.feature_current_label,
      feature_initial_status = x.feature_initial_status,
      feature_units = x.feature_units,
      initial_status = x.initial_status,
      round = x.round,
      icon = x.icon,
      feature_icon = x.feature_icon);
  }
  return x;
}
