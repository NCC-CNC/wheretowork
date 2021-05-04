class SingleThemeGroup {
  /* constructor */
  constructor(
      manager, id, name, feature_name, feature_id, feature_total_amount,
      feature_min_goal, feature_max_goal, feature_initial_goal,
      feature_step_goal, feature_limit_goal, feature_current_label,
      feature_current_held, feature_units, round, initial_status, icon) {
    // class fields
    this.el =
      document
      .getElementById("SingleThemeGoal-template").
      cloneNode(true);
    this.name_node = this.el.querySelector(".name-label");
    this.status_node = this.el.querySelector(".status-checkbox");
    this.goal_node = this.el.querySelector(".noUiSlider-widget");

    // local variables
    let that = this;
    let icon_node = this.el.querySelector(".icon");
    let goal_label_node = this.el.querySelector(".slider-label");
    let goal_symbol_node = this.el.querySelector(".slider-symbol");
    let current_label_node = this.el.querySelector(".current-label");
    let current_bar_node = this.el.querySelector(".current-bar");

    // attach metadata to DOM
    this.el.setAttribute("setting-id", id);
    this.el.setAttribute("setting-name", name);

    // set initial values
    /// icon
    icon_node.insertAdjacentHTML("beforeend", icon);
    /// name
    this.name_node.innerText = name;
    /// status
    this.status_node.checked = initial_status;
    /// goal
    noUiSlider.create(this.goal_node, {
      start: initial_goal,
      step: step_goal,
      connect: "lower",
      range: {
        "min": min_goal,
        "max": max_goal
      },
    });
    /// current label
    current_label_node.innerText =
      single_current_label_text(
        feature_current_held, feature_total_amount, round,
        feature_current_label, feature_units);
    /// current bar width
    style_current_bar(current_bar_node, feature_current_held);

    // set listeners to update user interfance
    if (HTMLWidgets.shinyMode) {
      /// enforce minimum limit
      this.goal_node.noUiSlider.on("change", function (values, handle) {
        if (values[handle] < feature_limit_goal) {
          that.goal_node.noUiSlider.set(feature_limit_goal);
        }
      });
      /// update goal label
      this.goal_node.noUiSlider.on("update", function (values, handle) {
        goal_label_node.innerText = single_goal_label_text(
          values[handle], feature_total_amount, round,
          "Goal", feature_units);
      });
      /// enable/disable widget on click
      this.status_node.addEventListener("change", function () {
        let checked = this.checked;
        if (checked) {
          that.name_node.removeAttribute("disabled");
          that.goal_node.removeAttribute("disabled");
          goal_label_node.removeAttribute("disabled");
          goal_symbol_node.removeAttribute("disabled");
          icon_node.firstChild.removeAttribute("disabled");
        } else {
          that.name_node.setAttribute("disabled", "");
          that.goal_node.setAttribute("disabled", "");
          goal_label_node.setAttribute("disabled", "");
          goal_symbol_node.setAttribute("disabled", "");
          icon_node.firstChild.setAttribute("disabled", "");
        }
      });
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// goal
      this.goal_node.noUiSlider.on("update", function (values, handle) {
        Shiny.setInputValue(manager + "_setting", {
          id = id,
          parameter = "theme",
          value = values[handle],
          type = "goal"
        });
      });
      /// status
      this.status_node.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager + "_setting", {
          id = id,
          parameter = "status",
          value = checked,
          type = "theme"
        });
      });
    }

  }

  /* update HTML components */
  updateName(value) {
    this.name_node.innerText = value;
  }

  updateStatus(value) {
    let event = document.createEvent("HTMLEvents");
    event.initEvent(".change", false, true);
    this.status_node.checked = value;
    this.status_node.dispatchEvent(event);
  }

  render() {
    return this.el;
  }

  /// functions for compatibility with multiThemeGoal
  updateView: function(value) {
    // for compatibility with functions for multiple themes
    // no effect because the widget doesn't support multiple views
  }

  updateGroupGoal: function(value) {
    // for compatibility with functions for multiple themes
    this.updateFeatureGoals(value);
  }

  updateFeatureGoals: function(value) {
    this.goal_node.noUiSlider.set(value[0]);
  }

  updateFeatureStatuses: function(value) {
    // for compatibility with functions for multiple themes
    this.updateStatus(value);
  }

};
