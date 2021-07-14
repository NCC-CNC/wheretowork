function newSolution(manager, x) {
  return new Solution(
    manager,
    x.id,
    x.name,
    x.statistics,
    x.theme_results,
    x.weight_results,
    x.solution_color
  );
}

function newStatistics(manager, x) {
  return new Statistics(
    manager,
    x.map((y) => y.name),
    x.map((y) => y.value),
    x.map((y) => y.units)
  );
}

function newWeightResults(manager, x, solution_color) {
  return new WeightResults(
    manager,
    x.id,
    x.name,
    x.status,
    x.factor,
    x.total_amount,
    x.current_held,
    x.solution_held,
    x.units,
    solution_color
  );
}

function newThemeResults(manager, x, solution_color) {
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
      x.icon,
      solution_color
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
      x.icon,
      solution_color
    );
  }
  return y;
}
