class MultiThemeSetting {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    feature_name,
    feature_id,
    feature_total_amount,
    feature_current_held,
    group_min_goal,
    group_max_goal,
    group_initial_goal,
    group_limit_goal,
    group_step_goal,
    group_current_label,
    feature_min_goal,
    feature_max_goal,
    feature_initial_goal,
    feature_limit_goal,
    feature_step_goal,
    feature_current_label,
    feature_initial_status,
    feature_icon,
    units,
    initial_status,
    round,
    icon
  ) {
    // class fields
    /// HTML container
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".multi-theme-setting-template")
        .content,
      true);

    /// internal variables
    this.n_features = feature_id.length;
    this.single_goal_values = feature_initial_goal;
    this.single_status_values = feature_initial_status;

    /// declare fields that correspond to HTML elements
    this.status_el = undefined;
    this.single_status_el = undefined;

    this.group_goal_el = undefined;
    this.single_goal_el = undefined;

    this.group_tab_el = undefined;
    this.single_tab_el = undefined;

    // local variables
    let that = this;

    /// theme HTML nodes
    let header_el = this.el.querySelector(".header");
    this.name_el = header_el.querySelector(".name-label");
    this.status_el = header_el.querySelector(".status-checkbox");
    let main_el = this.el.querySelector(".main");
    let icon_el = this.el.querySelector(".icon");
    let group_panel_el = main_el.querySelector(".group-view");
    let single_panel_el = main_el.querySelector(".single-view");

    /// group view HTML nodes
    let group_tab_el =
      this.el.querySelector("[data-value='group']");
    this.group_goal_el =
      group_panel_el.querySelector(".noUiSlider-widget");

    let group_goal_label_el =
      group_panel_el.querySelector(".slider-label");
    let group_goal_symbol_el =
      group_panel_el.querySelector(".slider-symbol");
    let group_current_label_el =
      group_panel_el.querySelector(".current-label");
    let group_current_min_bar_el =
      group_panel_el.querySelector(".current-min-bar");
    let group_current_max_bar_el =
      group_panel_el.querySelector(".current-max-bar");
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
    let single_icon_el =
      single_panel_el.querySelectorAll(".sub-icon");
    let single_current_label_el =
      single_panel_el.querySelectorAll(".current-label");
    let single_current_bar_el =
      single_panel_el.querySelectorAll(".current-bar");
    let single_widget_el =
      single_panel_el.querySelectorAll(".widget");

    // attach id to element
    this.el.querySelector(".solution-setting").id = id;
    for (let i = 0; i < this.n_features; ++i) {
      single_panel_el.children[i].id = feature_id[i];
    }

    // set initial theme values
    /// icon
    icon_el.insertAdjacentHTML("beforeend", icon);
    /// name
    this.name_el.innerText = name;
    /// status
    this.status_el.checked = initial_status;

    // set initial group values
    /// current text
    group_current_label_el.innerText =
      group_current_label_text(
        feature_current_held, feature_total_amount, round,
        group_current_label, units);
    /// style current bar
    style_group_current_bars(
      group_current_min_bar_el,
      group_current_max_bar_el,
      Math.min.apply(Math, feature_current_held),
      Math.max.apply(Math, feature_current_held));
    /// slider
    noUiSlider.create(this.group_goal_el, {
      start: group_initial_goal,
      step: group_step_goal,
      connect: "lower",
      range: {
        "min": group_min_goal,
        "max": group_max_goal
      },
    });

    // set initial single values
    for (let i = 0; i < this.n_features; ++i) {
      /// icon
      single_icon_el[i].insertAdjacentHTML("beforeend", feature_icon[i]);
      /// name text
      single_name_el[i].innerText = feature_name[i];
      /// status
      this.single_status_el[i].checked = feature_initial_status[i];
      /// current text
      single_current_label_el[i].innerText =
        single_current_label_text(
          feature_current_held[i], feature_total_amount[i],
          round, feature_current_label[i], units);
      /// current bar
      style_current_bar(
        single_current_bar_el[i], feature_current_held[i]);
      /// slider
      noUiSlider.create(this.single_goal_el[i], {
        start: feature_initial_goal[i],
        step: feature_step_goal[i],
        connect: "lower",
        range: {
          "min": feature_min_goal[i],
          "max": feature_max_goal[i]
        }
      });
    }

    // set listeners to update user interfance
    if (HTMLWidgets.shinyMode) {
      /// group view
      //// enforce minimum limit
      this.group_goal_el.noUiSlider.on('change', function (values, handle) {
        if (values[handle] < group_limit_goal) {
          that.group_goal_el.noUiSlider.set(group_limit_goal);
        }
      });
      //// update goal label
      this.group_goal_el.noUiSlider.on("update", function (values, handle) {
        group_goal_label_el.innerText =
          group_goal_label_text(
            values[handle], feature_total_amount, round,
            "Goal", units);
      });
      //// set status listener to enable/disable widget on click
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        that.single_status_el.forEach((x) => x.checked = checked);
        let els =
          document
          .getElementById(id)
          .querySelectorAll(
            `.disable-if-inactive, ` +
            `.disable-if-inactive.icon i, ` +
            `.disable-if-inactive.sub-icon i`);
        if (checked) {
          els.forEach((x) => x.removeAttribute("disabled"));
        } else {
          els.forEach((x) => x.setAttribute("disabled", ""));
        }
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
              values[handle], feature_total_amount[i], round,
              "Goal", units);
        });
        //// set status listener to enable/disable widget on click
        this.single_status_el[i].addEventListener("change", function () {
          let checked = this.checked;
          that.single_status_values[i] = checked;
          let els =
            document
            .getElementById(id)
            .querySelectorAll(
              `[id="${feature_id[i]}"] .disable-if-inactive, ` +
              `[id="${feature_id[i]}"] .disable-if-inactive.sub-icon i`);
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
      group_tab_el.addEventListener("click", function() {
        let checked = this.checked;
        let v = that.group_goal_el.noUiSlider.get();
        Shiny.setInputValue(manager, {
          id: id,
          parameter: "status",
          value: Array(that.n_features).fill(checked),
          type: "theme"
        });
        Shiny.setInputValue(manager, {
          id: id,
          parameter:"goal",
          value: Array(that.n_features).fill(v),
          type: "theme"
        });
      });
      //// status
      this.status_el.addEventListener("change", function () {
        let checked = this.checked;
        Shiny.setInputValue(manager, {
          id: id,
          parameter: "status",
          value: Array(that.n_features).fill(checked),
          type: "theme"
        });
      });
      //// goal
      this.group_goal_el.noUiSlider.on("update", function (values, handle) {
        let v = that.group_goal_el.noUiSlider.get();
        Shiny.setInputValue(manager, {
          id: id,
          parameter: "theme",
          value: Array(that.n_features).fill(v),
          type: "goal"
        });
      });

      /// single view
      for (let i = 0; i < this.n_features; ++i) {
        /// tab
        this.single_tab_el.addEventListener("click", function() {
          Shiny.setInputValue(manager, {
            id: id,
            parameter: "theme",
            value: that.single_status_values,
            type: "status"
          });
          Shiny.setInputValue(manager, {
            id: id,
            parameter: "theme",
            value: that.single_goal_values,
            type: "goal"
          });
        });
        /// status
        this.single_status_el[i].addEventListener("change", function() {
          that.single_status_values[i] = that.single_status_el[i].checked;
          Shiny.setInputValue(manager, {
            id: id,
            parameter: "theme",
            value: that.single_status_values,
            type: "status"
          });
        });
        //// slider
        this.single_goal_el[i].noUiSlider.on(
          "update", function (values, handle) {
          that.single_goal_values[i] = values[handle];
          Shiny.setInputValue(manager, {
            id: id,
            parameter: "theme",
            value: that.single_goal_values,
            type: "goal"
          });
        });
      }
    }
  }

  /* update HTML elements */
  updateName(value) {
    this.name_el.innerText = value;
  }

  updateStatus(value) {
    let event = document.createEvent("HTMLEvents");
    event.initEvent(".change", false, true);
    this.status_el.checked = value;
    this.status_el.dispatchEvent(event);
  }

  render() {
    return this.el;
  }

  updateView(value) {
    let event = document.createEvent("HTMLEvents");
    event.initEvent("click", false, true);
    if (value === "group") {
      this.group_tab_el.dispatchEvent(event);
    } else if (value === "single") {
      this.single_tab_el.dispatchEvent(event);
    }
  }

  updateGroupGoal(value) {
    this.group_goal_el.noUiSlider.set(value);
  }

  updateFeatureGoals(value) {
    for (let i = 0; i < this.n_features; ++i) {
      this.single_goal_el[i].noUiSlider.set(value[i]);
    }
  }

  updateFeatureStatuses(value) {
    // manually override group status
    if (value.value.some(x => x) && (!this.status_el.checked)) {
      this.updateStatus(true);
    }
    // update status variable
    this.single_status_values = value;
    // set elements as active/inactive
    let els = undefined;
    for (let i = 0; i < n_features; ++i) {
      this.single_status[i].checked = value[i];
      let els =
        document
        .getElementById(id)
        .querySelectorAll(
          `[id="${feature_id[i]}"] .disable-if-inactive, ` +
          `[id="${feature_id[i]}"] .disable-if-inactive.icon i`
        );
      if (value[i]) {
        els.forEach((x) => x.removeAttribute("disabled"));
      } else {
        els.forEach((x) => x.setAttribute("disabled", ""));
      }
    }
  }

};
