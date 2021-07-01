function newSolution(manager, x) {
  return new Solution(
    manager,
    x.id,
    x.name,
    x.statistics,
    x.theme_results,
    x.weight_results
  );
}

function newStatistic(manager, x) {
  return new Statistic(
    manager,
    x.name,
    x.value,
    x.units);
}

function newWeightResults(manager, x) {
  return new WeightResults(
    manager,
    x.id,
    x.name,
    x.status,
    x.total,
    x.factor,
    x.held,
    x.units,
  );
}

function newThemeResults(manager, x) {
  if (typeof(x.feature_name) === "string") {
    var y = new SingleThemeResults(
      manager,
      x.id,
      x.name,
      x.feature_name,
      x.feature_id,
      x.feature_status,
      x.feature_total_amount,
      x.feature_current_held,
      x.feature_goal,
      x.feature_solution_held,
      x.units,
      x.mandatory,
      x.round,
      x.icon
    );
  } else {
    var y = new MultiThemeResults(
      manager,
      x.id,
      x.name,
      x.feature_name,
      x.feature_id,
      x.feature_status,
      x.feature_total_amount,
      x.feature_current_held,
      x.feature_goal,
      x.feature_solution_held,
      x.units,
      x.mandatory,
      x.round,
      x.icon
    );
  }
  return y;
}
