class SingleThemeSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    feature_name,
    feature_id,
    feature_status,
    feature_total_amount,
    feature_current_held,
    feature_min_goal,
    feature_max_goal,
    feature_goal,
    feature_limit_goal,
    feature_step_goal,
    feature_provenance,
    units
  ) {
    // class fields
    this.id = id;
    this.elementId = "setting-" + id;
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
    this.goal_label_el = this.el.querySelector(".slider-label");
    this.goal_symbol_el = this.el.querySelector(".slider-symbol");
    this.reset_el = this.el.querySelector(".reset-button");
    this.current_label_el = this.el.querySelector(".current-label");
    this.current_bar_el = this.el.querySelector(".current-bar");
    this.limit = feature_limit_goal;
    this.total = feature_total_amount;
    this.units = units;
    this.previous_goal = feature_goal;

    // local variables
    let that = this;
    let goal_label_el = this.el.querySelector(".slider-label");
    let goal_symbol_el = this.el.querySelector(".slider-symbol");

    // attach id to element
    this.el.querySelector(".solution-setting").id = this.elementId;

    // set initial values
    /// name
    this.name_el.innerText = name;
    /// goal
    noUiSlider.create(this.goal_el, {
      start: feature_goal,
      step: feature_step_goal,
      connect: "lower",
      range: {
        "min": feature_min_goal,
        "max": feature_max_goal
      }
    });
    /// current label
    this.current_label_el.innerText =
      single_current_label_text(
        feature_current_held, this.total, "Current", this.units);
    /// current bar width
    style_current_bar(this.current_bar_el, feature_current_held);
    /// status
    this.updateStatus(feature_status);
    /// provenance
    createProvenance(
      this.el.querySelector(".provenance-container"), feature_provenance
    );

    // set listeners to update user interface
    if (HTMLWidgets.shinyMode) {
      /// enforce minimum limit
      this.goal_el.noUiSlider.on("change", function (values, handle) {
        if (values[handle] < that.limit) {
          that.goal_el.noUiSlider.set(that.limit);
        }
      });
      /// update goal label
      this.goal_el.noUiSlider.on("update", function (values, handle) {
        goal_label_el.innerText = single_goal_label_text(
          values[handle], that.total, "Goal", that.units);
      });
      /// enable/disable widget on click
      this.status_el.addEventListener("change", function () {
        //// set switch value
        let checked = this.checked;
        //// update slider
        if (checked) {
          that.goal_el.noUiSlider.set(that.previous_goal);
        } else {
          that.previous_goal = that.goal_el.noUiSlider.get();
          that.goal_el.noUiSlider.set(that.limit);
        }
        //// update HTML styles
        let els =
          document
          .getElementById(that.elementId)
          .querySelectorAll(".disable-if-inactive");
        if (checked) {
          els.forEach((x) => x.removeAttribute("disabled"));
        } else {
          els.forEach((x) => x.setAttribute("disabled", ""));
        }
      });
      /// reset button
      this.reset_el.addEventListener("click", function () {
        that.previous_goal = that.goal_el.noUiSlider.get();
        that.goal_el.noUiSlider.set(feature_goal);
      })
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// goal
      this.goal_el.noUiSlider.on("update", function (values, handle) {
        let v = parseFloat(values[handle]);
        if (v >= feature_limit_goal) {
          Shiny.setInputValue(manager, {
            id: id,
            setting: "feature_goal",
            value: v,
            type: "theme"
          });
        }
      });
      /// status
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          setting: "feature_status",
          value: checked,
          type: "theme"
        });
      });
    }
  }

  /* update methods */
  updateSetting(setting, value) {
    if (setting === "name") {
      this.updateName(value);
    } else if (setting === "status" || setting === "feature_status") {
      this.updateStatus(value);
    } else if (setting === "feature_goal") {
      this.updateFeatureGoal(value);
    } else if (setting === "feature_current") {
      this.updateFeatureCurrent(value);
    }
  }

  updateName(value) {
    this.name_el.innerText = value;
  }

  updateStatus(value) {
    // update HTML elements if needed
    if (this.status_el.checked !== value) {
      /// update switch;
      this.status_el.checked = value;
      /// update slider
      if (value) {
        this.name_el.removeAttribute("disabled");
        this.goal_el.removeAttribute("disabled");
        this.goal_label_el.removeAttribute("disabled");
        this.goal_symbol_el.removeAttribute("disabled");
        this.reset_el.removeAttribute("disabled");
        this.goal_el.noUiSlider.set(this.previous_goal);
      } else {
        this.name_el.setAttribute("disabled");
        this.goal_el.setAttribute("disabled");
        this.goal_label_el.setAttribute("disabled");
        this.goal_symbol_el.setAttribute("disabled");        
        this.reset_el.setAttribute("disabled");
        this.previous_goal = this.goal_el.noUiSlider.get();
        this.goal_el.noUiSlider.set(this.limit);
      }
    }
    // update HTML element styles
    let els = this.el.querySelectorAll(".disable-if-inactive");
    if (value) {
      els.forEach((x) => x.removeAttribute("disabled"));
    } else {
      els.forEach((x) => x.setAttribute("disabled", ""));
    }
  }

  updateFeatureGoal(value) {
    this.previous_goal = value;
    this.goal_el.noUiSlider.set(value);
  }

  /* dummy methods included for compatibility with MultiThemeSetting  */
  updateView(value) {
    // no effect
  }

  updateFeatureStatus(value) {
    self.updateStatus(value);
  }

  updateFeatureCurrent(value) {
    // current label
    this.current_label_el.innerText =
      single_current_label_text(value, this.total, "Current", this.units);
    // current bar width
    style_current_bar(this.current_bar_el, value);
  }

  /* render method */
  render() {
    return this.el;
  }

};
