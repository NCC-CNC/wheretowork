class MultiThemeSetting {
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
    units
  ) {
    // class fields
    /// internal variables
    this.id = id;
    this.elementId = "setting-" + id;
    this.n_features = feature_id.length;
    this.feature_id = feature_id.map((x) => "setting-" + x);
    this.single_goal_values = [...feature_goal];
    this.single_status_values = feature_status.map((x) => x);
    this.single_total_values = feature_total_amount;
    this.single_limit_values = feature_limit_goal;
    this.units = units;
    this.group_limit_goal = Math.max.apply(Math, feature_limit_goal);
    this.previous_group_goal = Math.max.apply(Math, feature_goal);
    this.previous_single_goals = [...feature_goal];

    /// HTML container
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".multi-theme-setting-template")
        .content,
      true);

    /// declare fields that correspond to HTML elements
    this.status_el = undefined;
    this.single_status_el = undefined;

    this.group_goal_el = undefined;
    this.single_goal_el = undefined;

    this.group_current_label_el = undefined;
    this.single_current_label_el = undefined;

    this.group_current_min_bar_el = undefined;
    this.group_current_max_bar_el = undefined;
    this.single_current_bar_el = undefined;

    this.group_tab_el = undefined;
    this.single_tab_el = undefined;

    // local variables
    let that = this;

    /// theme HTML nodes
    let header_el = this.el.querySelector(".header");
    this.name_el = header_el.querySelector(".name-label");
    this.status_el = header_el.querySelector(".status-checkbox");
    this.reset_el = header_el.querySelector(".reset-button");
    let main_el = this.el.querySelector(".main");
    let group_panel_el = main_el.querySelector(".group-view");
    let single_panel_el = main_el.querySelector(".single-view");

    /// group view HTML nodes
    this.group_tab_el =
      this.el.querySelector("[data-value='group']");
    this.group_goal_el =
      group_panel_el.querySelector(".noUiSlider-widget");
    this.group_current_label_el =
      group_panel_el.querySelector(".current-label");
    this.group_current_min_bar_el =
      group_panel_el.querySelector(".current-min-bar");
    this.group_current_max_bar_el =
      group_panel_el.querySelector(".current-max-bar");

    let group_goal_label_el =
      group_panel_el.querySelector(".slider-label");
    let group_goal_symbol_el =
      group_panel_el.querySelector(".slider-symbol");
    let group_widget_el_el =
      group_panel_el.querySelector(".widget");

    /// attach single view nodes DOM elements for each feautre
    for (let i = 0; i < this.n_features; ++i) {
      single_panel_el.appendChild(
        document.importNode(
          document
          .getElementById(manager)
          .querySelector(".multi-theme-single-setting-template")
          .content,
        true));
    }

    /// single view HTML nodes
    this.single_tab_el =
      this.el.querySelector("[data-value='single']");
    let single_name_el =
      single_panel_el.querySelectorAll(".name-label");
    this.single_status_el =
      single_panel_el.querySelectorAll(".status-checkbox");
    this.single_goal_el =
      single_panel_el.querySelectorAll(".noUiSlider-widget");
    let single_goal_label_el =
      single_panel_el.querySelectorAll(".slider-label");
    this.single_current_label_el =
      single_panel_el.querySelectorAll(".current-label");
    this.single_current_bar_el =
      single_panel_el.querySelectorAll(".current-bar");
    let single_widget_el =
      single_panel_el.querySelectorAll(".widget");

    // attach id to elements
    /// main container
    this.el.querySelector(".solution-setting").id = this.elementId;
    /// single view containers
    for (let i = 0; i < this.n_features; ++i) {
      single_panel_el.children[i].id = this.feature_id[i];
    }
    /// tab containers
    this.el
      .querySelector(".tabbable ul")
      .setAttribute("data-tabsetid", `tabs-${this.elementId}`);
    this.el
      .querySelector(".tabbable .tab-content")
      .setAttribute("data-tabsetid", `tabs-${this.elementId}`);
    /// group tab
    this.group_tab_el.setAttribute("href", `#tabs-${this.elementId}-1`);
    this.el
      .querySelector(".tabbable .tab-content [data-value='group']")
      .setAttribute("id", `tabs-${this.elementId}-1`);
    /// single tab
    this.single_tab_el.setAttribute("href", `#tabs-${this.elementId}-2`);
    this.el
      .querySelector(".tabbable .tab-content [data-value='single']")
      .setAttribute("id", `tabs-${this.elementId}-2`);

    // set initial theme values
    /// name
    this.name_el.innerText = name;
    // set initial group values
    /// current text
    this.group_current_label_el.innerText =
      group_current_label_text(
        feature_current_held, this.single_total_values,
        "Current", this.units);
    /// style current bar
    style_group_current_bars(
      this.group_current_min_bar_el,
      this.group_current_max_bar_el,
      Math.min.apply(Math, feature_current_held),
      Math.max.apply(Math, feature_current_held));
    /// slider
    noUiSlider.create(this.group_goal_el, {
      start: Math.max.apply(Math, feature_goal),
      step: Math.min.apply(Math, feature_step_goal),
      connect: "lower",
      range: {
        "min": Math.min.apply(Math, feature_min_goal),
        "max": Math.min.apply(Math, feature_max_goal)
      },
    });

    // set initial single values
    for (let i = 0; i < this.n_features; ++i) {
      /// name text
      single_name_el[i].innerText = feature_name[i];
      /// current text
      this.single_current_label_el[i].innerText =
        single_current_label_text(
          feature_current_held[i],
          this.single_total_values[i],
          "Current",
          this.units);
      /// current bar
      style_current_bar(
        this.single_current_bar_el[i], feature_current_held[i]);
      /// slider
      noUiSlider.create(this.single_goal_el[i], {
        start: feature_goal[i],
        step: feature_step_goal[i],
        connect: "lower",
        range: {
          "min": feature_min_goal[i],
          "max": feature_max_goal[i]
        }
      });
    }

    // set status
    this.updateFeatureStatus([...feature_status]);
    if (!feature_status.some(x => x)) {
      this.updateStatus(false);
    }

    // set listeners to update user interface
    if (HTMLWidgets.shinyMode) {
      /// group view
      //// enforce minimum limit
      this.group_goal_el.noUiSlider.on("change", function (values, handle) {
        if (values[handle] < that.group_limit_goal) {
          that.group_goal_el.noUiSlider.set(that.group_limit_goal);
        }
      });
      //// update goal label
      this.group_goal_el.noUiSlider.on("update", function (values, handle) {
        group_goal_label_el.innerText =
          group_goal_label_text(
            values[handle], that.single_total_values, "Goal", that.units);
      });
      //// set status listener to enable/disable widget on click
      this.status_el.addEventListener("change", function () {
        ///// set switch values
        let checked = this.checked;
        ///// update group slider
        if (checked) {
          that.group_goal_el.noUiSlider.set(that.previous_group_goal);
        } else {
          that.previous_group_goal = that.group_goal_el.noUiSlider.get();
          that.group_goal_el.noUiSlider.set(that.group_limit_goal);
        }
        ///// update single sliders
        for (let i = 0; i < that.n_features; ++i) {
          if (that.single_status_el[i].checked !== checked) {
            /// update switch
            that.single_status_el[i].checked = checked;
            /// update slider
            if (checked) {
              that.single_goal_el[i].noUiSlider.set(
                that.previous_single_goals[i]);
            } else {
              that.previous_single_goals[i] =
                that.single_goal_el[i].noUiSlider.get();
              that.single_goal_el[i].noUiSlider.set(
                that.single_limit_values[i]);
            }
          }
        }
        //// update HTML element styles
        let els =
          document
          .getElementById(that.elementId)
          .querySelectorAll(
            `.disable-if-inactive, ` +
            `.sub-header .status-checkbox`);
        if (checked) {
          els.forEach((x) => x.removeAttribute("disabled"));
        } else {
          els.forEach((x) => x.setAttribute("disabled", ""));
        }
      });
      //// reset button
      this.reset_el.addEventListener("click", function() {
        //// reset status
        that.updateFeatureStatus(feature_status);
        /// reset goals
        that.updateGroupGoal(Math.max.apply(Math, feature_goal));
        that.updateFeatureGoal(feature_goal);
        /// pass status data to Shiny
        Shiny.setInputValue(manager, {
          id: id,
          setting: "feature_status",
          value: feature_status,
          type: "theme"
        });
        /// pass goal data to Shiny (note we use a timeout all data is sent)
        setTimeout(
          function() {
            Shiny.setInputValue(manager, {
              id: id,
              setting: "feature_goal",
              value: feature_goal,
              type: "theme"
            });
          },
          200
        );
      });
      /// single view
      for (let i = 0; i < this.n_features; ++i) {
        //// enforce minimum limit
        this.single_goal_el[i].noUiSlider.on(
          "change", function (values, handle) {
          if (values[handle] < feature_limit_goal[i]) {
            that.single_goal_el[i].noUiSlider.set(feature_limit_goal[i]);
          }
        });
        //// update goal label
        this.single_goal_el[i].noUiSlider.on(
          "update", function (values, handle) {
          single_goal_label_el[i].innerText =
            single_goal_label_text(
              values[handle], that.single_total_values[i],
              "Goal", that.units);
        });
        //// set status listener to enable/disable widget on click
        this.single_status_el[i].addEventListener("change", function () {
          ///// set switch values
          let checked = this.checked;
          ///// update switches
          that.single_status_values[i] = checked;
          ///// update slider
          if (checked) {
            that.single_goal_el[i].noUiSlider.set(
              that.previous_single_goals[i]);
          } else {
            that.previous_single_goals[i] =
              that.single_goal_el[i].noUiSlider.get();
            that.single_goal_el[i].noUiSlider.set(
              that.single_limit_values[i]);
          }
          //// update HTML element styles
          let els =
            document
            .getElementById(that.elementId)
            .querySelectorAll(
              `[id="${that.feature_id[i]}"] .disable-if-inactive`);
          if (checked) {
            els.forEach((x) => x.removeAttribute("disabled"));
          } else {
            els.forEach((x) => x.setAttribute("disabled", ""));
          }
        });
      }
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// group view
      //// tab
      this.group_tab_el.addEventListener("click", function() {
        let checked = this.checked;
        let v = that.group_goal_el.noUiSlider.get();
        Shiny.setInputValue(manager, {
          id: id,
          setting: "feature_status",
          value: Array(that.n_features).fill(checked),
          type: "theme"
        });
        Shiny.setInputValue(manager, {
          id: id,
          setting:"feature_goal",
          value: Array(that.n_features).fill(parseFloat(v)),
          type: "theme"
        });
      });
      //// status
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          setting: "feature_status",
          value: Array(that.n_features).fill(checked),
          type: "theme"
        });
      });
      //// goal
      this.group_goal_el.noUiSlider.on("update", function (values, handle) {
        let v = parseFloat(values[handle]);
        if (v >= that.group_limit_goal) {
          Shiny.setInputValue(manager, {
            id: id,
            setting: "feature_goal",
            value: Array(that.n_features).fill(v),
            type: "theme"
          });
        }
      });

      /// single view
      for (let i = 0; i < this.n_features; ++i) {
        /// tab
        this.single_tab_el.addEventListener("click", function() {
          Shiny.setInputValue(manager, {
            id: id,
            setting: "status",
            value: that.single_status_values,
            type: "feature_status"
          });
          Shiny.setInputValue(manager, {
            id: id,
            setting: "feature_goal",
            value: that.single_goal_values,
            type: "theme"
          });
        });
        /// status
        this.single_status_el[i].addEventListener("change", function() {
          that.single_status_values[i] = that.single_status_el[i].checked;
          Shiny.setInputValue(manager, {
            id: id,
            setting: "feature_status",
            value: that.single_status_values,
            type: "theme"
          });
        });
        //// slider
        this.single_goal_el[i].noUiSlider.on(
          "update", function (values, handle) {
          let v = parseFloat(values[handle]);
          if (v >= feature_limit_goal[i]) {
            that.single_goal_values[i] = v;
            Shiny.setInputValue(manager, {
              id: id,
              setting: "feature_goal",
              value: that.single_goal_values,
              type: "theme"
            });
          }
        });
      }
    }
  }

  /* update HTML elements */
  /* update methods */
  updateSetting(setting, value) {
    if (setting === "name") {
      this.updateName(value);
    } else if (setting === "status") {
      this.updateStatus(value);
    } else if (setting === "view") {
      this.updateView(value);
    } else if (setting === "group_goal") {
      this.updateGroupGoal(value);
    } else if (setting === "feature_status") {
      this.updateFeatureStatus(value);
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
    // update group HTML elements if needed
    if (this.status_el.checked !== value) {
      /// update switch
      this.status_el.checked = value;
      /// update group slider if needed
      if (value) {
        this.group_goal_el.noUiSlider.set(this.previous_group_goal);
      } else {
        this.previous_group_goal = this.group_goal_el.noUiSlider.get();
        this.group_goal_el.noUiSlider.set(this.group_limit_goal);
      }
    }
    /// update single HTML elements if needed
    for (let i = 0; i < this.n_features; ++i) {
      if (this.single_status_el[i].checked !== value) {
        /// update switch
        this.single_status_el[i].checked = value;
        /// update slider
        if (value) {
          this.single_goal_el[i].noUiSlider.set(
            this.previous_single_goals[i]);
        } else {
          this.previous_single_goals[i] =
            this.single_goal_el[i].noUiSlider.get();
          this.single_goal_el[i].noUiSlider.set(
            this.single_limit_values[i]);
        }
      }
    }
    // update HTML element styles
    this.single_status_values.fill(value);
    let els = this.el.querySelectorAll(
        `.disable-if-inactive, ` +
        `.sub-header .status-checkbox`);
    if (value) {
      els.forEach((x) => x.removeAttribute("disabled"));
    } else {
      els.forEach((x) => x.setAttribute("disabled", ""));
    }
  }

  updateView(value) {
    if (value == "group") {
      this.group_tab_el.click();
    } else if (value == "single") {
      this.single_tab_el.click();
    }
  }

  updateGroupGoal(value) {
    this.previous_group_goal = value;
    this.group_goal_el.noUiSlider.set(value);
  }

  updateFeatureStatus(value) {
    // manually override group status
    if (value.some(x => x) && (this.status_el.checked !== true)) {
      this.updateStatus(true);
    }
    // update status variable
    this.single_status_values = [...value];
    // iterate over each feature
    let els = undefined;
    for (let i = 0; i < this.n_features; ++i) {
      /// update if needed
      if (this.single_status_el[i].checked !== value[i]) {
        /// update switch
        this.single_status_el[i].checked = value[i];
        /// update slider
        if (value[i]) {
          this.single_goal_el[i].noUiSlider.set(this.previous_single_goals[i]);
        } else {
          this.previous_single_goals[i] =
            this.single_goal_el[i].noUiSlider.get();
          this.single_goal_el[i].noUiSlider.set(
            this.single_limit_values[i]);
        }
      }
      /// update HTML element styles
      els = document.querySelectorAll(
        `[id="${this.feature_id[i]}"] .disable-if-inactive`);
      if (value[i]) {
        els.forEach((x) => x.removeAttribute("disabled"));
      } else {
        els.forEach((x) => x.setAttribute("disabled", ""));
      }
    }
  }

  updateFeatureGoal(value) {
    this.previous_single_goals = [...value];
    for (let i = 0; i < this.n_features; ++i) {
      this.single_goal_el[i].noUiSlider.set(value[i]);
    }
  }

  updateFeatureCurrent(value) {
    // update group view components
    /// label
    this.group_current_label_el.innerText =
      group_current_label_text(
        value, this.single_total_values,
        "Current", this.units);
    /// bars
    style_group_current_bars(
      this.group_current_min_bar_el,
      this.group_current_max_bar_el,
      Math.min.apply(Math, value),
      Math.max.apply(Math, value));

    // update single view components
    for (let i = 0; i < this.n_features; ++i) {
      /// label
      this.single_current_label_el[i].innerText =
        single_current_label_text(
          value[i], this.single_total_values[i],
          "Current", this.units);
      /// bar
      style_current_bar(this.single_current_bar_el[i], value[i]);
    }
  }

  /* render method */
  render() {
    return this.el;
  }

};
