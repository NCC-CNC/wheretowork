class SingleThemeSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    feature_name,
    feature_id,
    feature_total_amount,
    feature_current_held,
    feature_min_goal,
    feature_max_goal,
    feature_initial_goal,
    feature_limit_goal,
    feature_step_goal,
    feature_current_label,
    units,
    initial_status,
    round,
    icon
  ) {
    // class fields
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".single-theme-setting-template")
        .content,
      true);
    this.name_el = this.el.querySelector(".name-label");
    this.status_el = this.el.querySelector(".status-checkbox");
    this.goal_el = this.el.querySelector(".noUiSlider-widget");

    // local variables
    let that = this;
    let icon_el = this.el.querySelector(".icon");
    let goal_label_el = this.el.querySelector(".slider-label");
    let goal_symbol_el = this.el.querySelector(".slider-symbol");
    let current_label_el = this.el.querySelector(".current-label");
    let current_bar_el = this.el.querySelector(".current-bar");

    // attach id to element
    this.el.querySelector(".solution-setting").id = id;

    // set initial values
    /// icon
    icon_el.insertAdjacentHTML("beforeend", icon);
    /// name
    this.name_el.innerText = name;
    /// status
    this.status_el.checked = initial_status;
    /// goal
    noUiSlider.create(this.goal_el, {
      start: feature_initial_goal,
      step: feature_step_goal,
      connect: "lower",
      range: {
        "min": feature_min_goal,
        "max": feature_max_goal
      }
    });
    /// current label
    current_label_el.innerText =
      single_current_label_text(
        feature_current_held, feature_total_amount, round,
        feature_current_label, units);
    /// current bar width
    style_current_bar(current_bar_el, feature_current_held);

    // set listeners to update user interfance
    if (HTMLWidgets.shinyMode) {
      /// enforce minimum limit
      this.goal_el.noUiSlider.on("change", function (values, handle) {
        if (values[handle] < feature_limit_goal) {
          that.goal_el.noUiSlider.set(feature_limit_goal);
        }
      });
      /// update goal label
      this.goal_el.noUiSlider.on("update", function (values, handle) {
        goal_label_el.innerText = single_goal_label_text(
          values[handle], feature_total_amount, round,
          "Goal", units);
      });
      /// enable/disable widget on click
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        let els =
          document.getElementById(id).querySelectorAll(
            ".disable-if-inactive, .disable-if-inactive.icon i");
        if (checked) {
          els.forEach((x) => x.removeAttribute("disabled"));
        } else {
          els.forEach((x) => x.setAttribute("disabled", ""));
        }
      });
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// goal
      this.goal_el.noUiSlider.on("update", function (values, handle) {
        Shiny.setInputValue(manager, {
          id: id,
          parameter: "feature_goal",
          value: parseFloat(values[handle]),
          type: "theme"
        });
      });
      /// status
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          parameter: "feature_status",
          value: checked,
          type: "theme"
        });
      });
    }
  }

  /* update methods */
  updateParameter(parameter, value) {
    if (parameter === "name") {
      this.updateName(value);
    } else if (parameter === "status" || parameter === "feature_status") {
      this.updateStatus(value);
    } else if (parameter === "feature_goal") {
      this.updateFeatureGoal(value);
    }
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  updateStatus(value) {
    this.status_el.checked = value;
    let els =
      document.getElementById(this.id).querySelectorAll(
        ".disable-if-inactive, .disable-if-inactive.icon i");
    if (value) {
      els.forEach((x) => x.removeAttribute("disabled"));
    } else {
      els.forEach((x) => x.setAttribute("disabled", ""));
    }
  }

  updateFeatureGoal(value) {
    this.goal_el.noUiSlider.set(value);
  }

  /* dummy methods included for compatibility with MultiThemeSetting  */
  updateView(value) {
    // no effect
  }
  updateFeatureStatus(value) {
    self.updateStatus(value);
  }

  /* render method */
  render() {
    return this.el;
  }

};
