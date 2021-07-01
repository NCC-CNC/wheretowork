// display text indicating the current amount of a single feature held
function single_current_label_text(prop, total, prefix, units) {
  let v1 = Math.round(prop * 100.0);
  let v2 = auto_round(prop * total);
  return `${prefix}: ${v1}% (${v2} ${units})`;
}

// display text indicating the gobal for a single feature held
function single_goal_label_text(prop, total, prefix, units) {
  let v1 = Math.round(prop * 100.0);
  let v2 = auto_round(prop * total);
  return `${prefix}: ${v1}% (${v2} ${units})`;
}

// display text indicating the current amount of a group of features
function group_current_label_text(prop, total, prefix, units) {
  // run calculations
  let v1 = Math.min.apply(Math, prop) * 100.0;
  let v2_totals = new Array(prop.length);
  for (let i = 0; i < prop.length; i++) {
    v2_totals[i] = prop[i] * total[i];
  }
  let v2 = Math.min.apply(Math, v2_totals);
  // round numbers
  v1 = Math.round(v1);
  v2 = auto_round(v2);
  // return result
  return `${prefix}: ≥${v1}% (≥${v2} ${units})`;
}

// display text indicating the goal for a group of features
function group_goal_label_text(prop, total, prefix, units) {
  // run calculations
  let v1 = prop * 100.0;
  let v2_totals = new Array(total.length);
  for (let i = 0; i < total.length; i++) {
    v2_totals[i] = prop * total[i];
  }
  let v2 = Math.min.apply(Math, v2_totals);
  // round numbers
  v1 = Math.round(v1);
  v2 = auto_round(v2);
  // return result
  return `${prefix}: ≥${v1}% (≥${v2} ${units})`;
}

// style current bar
function style_current_bar(d, prop, threshold = 0) {
  let w = prop;
  w = Math.max(w, threshold);
  d.style.width = (w * 100) + "%";
}

// style group bars
function style_group_current_bars(
  d_min, d_max, min, max, threshold = 0) {
  // calculate sizes
  let minw = min;
  let maxw = max;
  // apply thresholds
  minw = Math.max(minw, threshold);
  maxw = Math.max(maxw, threshold);
  // resize bars
  d_min.style.width = (minw * 100) + "%";
  d_max.style.width = (maxw * 100) + "%";
  // set color for max bar
  let col = window.getComputedStyle(d_max).backgroundColor;
  d_max.style.background =
    `linear-gradient(to right, ${col} 0, ${col} ` +
    `${minw}px, transparent 100%)`;
}
