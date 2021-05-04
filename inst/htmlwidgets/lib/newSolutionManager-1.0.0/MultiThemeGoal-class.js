class MultiThemeGroup {
  /* constructor */
  constructor(
    manager = manager, id, feature_name, feature_id, feature_total_amount,
    feature_current_held, group_min_goal, group_max_goal, group_initial_goal,
    group_limit_goal, group_step_goal, group_current_label, feature_min_goal,
    feature_max_goal, feature_initial_goal, feature_limit_goal,
    feature_step_goal, feature_current_label, feature_initial_status,
    feature_units, initial_status, round, icon, feature_icon) {
    // class fields
    this.el =
      document.getElementById("MultiThemeGoal-template").cloneNode(true);
    this.single_goal_values = feature_initial_goal;
    this.single_status_values = feature_initial_status;
    this.n_features = feature_id.length;

    this.status_node = undefined;

    this.group_tab_node = undefined;
    this.group_goal_node = undefined;

    this.single_tab_node = undefined;
    this.single_name_node = undefined;
    this.single_goal_node = undefined;
    this.single_goal_label_node = undefined;
    this.single_goal_symbol_node = undefined;
    this.single_status_node = undefined;
    this.single_icon_node = undefined;

    // local variables
    let that = this;

    /// theme HTML nodes
    let header_node = this.el.querySelector(".header");
    this.name_node = header_node.querySelector(".name-label");
    this.status_node = header_node.querySelector(".status-checkbox");
    let main_node = this.el.querySelector(".main");
    let icon_node = this.el.querySelector(".icon");
    let group_panel_node = main_node.querySelector(".group-view");
    let single_panel_node = main_node.querySelector(".single-view");

    /// group view HTML nodes
    this.group_tab_node =
      this.el.querySelector("[data-value='group']");
    this.group_goal_node =
      group_panel_node.querySelector(".noUiSlider-widget");
    let group_goal_label_node =
      group_panel_node.querySelector(".slider-label");
    let group_goal_symbol_node =
      group_panel_node.querySelector(".slider-symbol");
    let group_current_label_node =
      group_panel_node.querySelector(".current-label");
    let group_current_min_bar_node =
      group_panel_node.querySelector(".current-min-bar");
    let group_current_max_bar_node =
      group_panel_node.querySelector(".current-max-bar");
    let group_widget_node_node =
      group_panel_node.querySelector(".widget");

    /// attach single view nodes DOM elements for each feautre
    for (let i = 0; i < this.n_features; ++i) {
      single_panel_node.appendChild(
        document
        .getElementById("MultiThemeGoal-single-template")
        .cloneNode(true));
    }

    /// single view HTML nodes
    this.single_tab_node =
      this.el.querySelector("[data-value='single']");
    this.single_name_node =
      single_panel_node.querySelectorAll(".name-label");
    this.single_status_node =
      single_panel_node.querySelectorAll(".status-checkbox");
    this.single_goal_node =
      single_panel_node.querySelectorAll(".noUiSlider-widget");
    this.single_goal_label_node =
      single_panel_node.querySelectorAll(".slider-label");
    this.single_goal_symbol_node =
      single_panel_node.querySelectorAll(".slider-symbol");
    this.single_icon_node =
      single_panel_node.querySelectorAll(".sub-icon");
    let single_current_label_node =
      single_panel_node.querySelectorAll(".current-label");
    let single_current_bar_node =
      single_panel_node.querySelectorAll(".current-bar");
    let single_widget_node =
      single_panel_node.querySelectorAll(".widget");

    // attach metadata to DOM
    this.el.setAttribute("setting-id", id);
    this.el.setAttribute("setting-name", name);

    // set initial theme values
    /// icon
    icon_node.insertAdjacentHTML("beforeend", icon);
    /// name
    name_node.innerText = name;
    /// status
    this.status_node.checked = initial_status;

    // set initial group values
    /// current text
    group_current_label_node.innerText =
      group_current_label_text(
        feature_current_held, feature_total_amount, round,
        group_current_label, feature_units);
    /// style current bar
    style_group_current_bars(
      group_current_min_bar_node,
      group_current_max_bar_node,
      Math.min.apply(Math, feature_current_held),
      Math.max.apply(Math, feature_current_held));
    /// slider
    noUiSlider.create(group_goal_node, {
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
      this.single_icon_node[i].insertAdjacentHTML(
        "beforeend", feature_icon[i]);
      /// name text
      this.single_name_node[i].innerText = feature_name[i];
      /// status
      this.single_status_node[i].checked = feature_initial_status[i];
      /// current text
      single_current_label_node[i].innerText =
        single_current_label_text(
          feature_current_held[i], feature_total_amount[i],
          round, feature_current_label[i], feature_units);
      /// current bar
      style_current_bar(
        single_current_bar[i], feature_current_held[i]);
    }

    // set listeners to update user interfance
    if (HTMLWidgets.shinyMode) {
      /// group view
      //// enforce minimum limit
      group_goal_node.noUiSlider.on('change', function (values, handle) {
        if (values[handle] < group_limit_goal) {
          group_goal_node.noUiSlider.set(group_limit_goal);
        }
      });
      //// update goal label
      group_goal_node.noUiSlider.on("update", function (values, handle) {
        group_goal_label_node.innerText =
          group_goal_label_text(
            values[handle], feature_total_amount, round,
            "Goal", feature_units);
      });
      //// set status listener to enable/disable widget on click
      this.status_node.addEventListener("change", function () {
        let checked = this.checked;
        if (checked) {
          name_node.removeAttribute("disabled");
          group_goal_node.removeAttribute("disabled");
          group_goal_label_node.removeAttribute("disabled");
          group_goal_symbol_node.removeAttribute("disabled");
          icon_node.firstChild.removeAttribute("disabled");
        } else {
          name_node.setAttribute("disabled", "");
          group_goal_node.setAttribute("disabled", "");
          group_goal_label_node.setAttribute("disabled", "");
          group_goal_symbol_node.setAttribute("disabled", "");
          icon_node.firstChild.setAttribute("disabled", "");
        }
        for (let i = 0; i < that.n_features; ++i) {
          this.toggleSingle(i, checked);
        }
      });
      /// single view
      for (let i = 0; i < this.n_features; ++i) {
        //// enforce minimum limit
        this.single_goal_node[i].noUiSlider.on(
          "change", function (values, handle) {
          if (values[handle] < feature_limit_goal[i]) {
            that.single_goal_node[i].noUiSlider.set(feature_limit_goal[i]);
          }
        });
        //// update goal label
        this.single_goal_node[i].noUiSlider.on(
          "update", function (values, handle) {
          that.single_goal_label_node[i].innerText =
            single_goal_label_text(
              values[handle], feature_total_amount[i], round,
              "Goal", feature_units);
        });
        //// set status listener to enable/disable widget on click
        this.single_status_node[i].addEventListener("change", function () {
          let checked = this.checked;
          that.single_status_values[i] = checked;
          this.toggleSingle(i, checked);
        });
      }
    }

    // set listeners to pass data to Shiny
    if (HTMLWidgets.shinyMode) {
      /// group view
      //// tab
      this.group_tab_node.addEventListener("click", function() {
        Shiny.setInputValue(manager + "_setting", {
          id = id,
          parameter = "status",
          value = Array(that.n_features).fill(status.checked),
          type = "theme"
        });
        Shiny.setInputValue(manager + "_setting", {
          id = id,
          parameter = "goal",
          value = Array(that.n_features).fill(group_goal.noUiSlider.get()),
          type = "theme"
        });
      });
      //// status
      this.status_node.addEventListener("change", function () {
        Shiny.setInputValue(manager + "_setting", {
          id = id,
          parameter = "status",
          value = Array(that.n_features).fill(status.checked),
          type = "theme"
        });
      });
      //// goal
      group_goal_node.noUiSlider.on("update", function (values, handle) {
        Shiny.setInputValue(manager + "_setting", {
          id = id,
          parameter = "theme",
          value = Array(that.n_features).fill(group_goal_node.noUiSlider.get()),
          type = "goal"
        });
      });

      /// single view
      for (let i = 0; i < this.n_features; ++i) {
        /// tab
        this.single_tab_node.addEventListener("click", function() {
          Shiny.setInputValue(manager + "_setting", {
            id = id,
            parameter = "theme",
            value = that.single_status_values,
            type = "status"
          });
          Shiny.setInputValue(manager + "_setting", {
            id = id,
            parameter = "theme",
            value = that.single_goal_values,
            type = "goal"
          });
        });
        /// status
        this.single_status_node[i].addEventListener("change", function() {
          that.single_status_values[i] = that.single_status_node[i].checked;
          Shiny.setInputValue(manager + "_setting", {
            id = id,
            parameter = "theme",
            value = that.single_status_values,
            type = "status"
          });
        });
        //// slider
        single_goal_node[i].noUiSlider.on("update", function (values, handle) {
          that.single_goal_values[i] = values[handle];
          Shiny.setInputValue(manager + "_setting", {
            id = id,
            parameter = "theme",
            value = that.single_goal_values,
            type = "goal"
          });
        });
      }
    }
  }

  /* enable/disable HTML elements */
  toggleSingle(i, checked) {
    if (checked) {
      this.single_name_node[i].removeAttribute("disabled");
      that.single_goal_node[i].removeAttribute("disabled");
      this.single_goal_label_node[i].removeAttribute("disabled");
      this.single_goal_symbol_node[i].removeAttribute("disabled");
      this.single_icon_node[i].firstChild.removeAttribute("disabled");
    } else {
      this.single_name_node[i].setAttribute("disabled", "");
      that.single_goal_node[i].setAttribute("disabled", "");
      this.single_goal_label_nodel[i].setAttribute("disabled", "");
      this.single_goal_symbol_node[i].setAttribute("disabled", "");
      this.single_icon_node[i].firstChild.setAttribute("disabled", "");
    }
  }

  /* update HTML elements */
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

  updateView: function(value) {
    let event = document.createEvent("HTMLEvents");
    event.initEvent("click", false, true);
    if (value === "group") {
      this.group_tab_node.dispatchEvent(event);
    } else if (value === "single") {
      this.single_tab_node.dispatchEvent(event);
    }
  }

  updateGroupGoal : function(value) {
    this.group_goal_node.noUiSlider.set(value);
  }

  updateFeatureGoals : function(value) {
    for (let i = 0; i < this.n_features; ++i) {
      this.single_goal_node[i].noUiSlider.set(value[i]);
    }
  }

  updateFeatureStatuses : function(value) {
    // manually override group status
    if (value.value.some(x => x) && (!this.status_node.checked)) {
      this.updateStatus(true);
    }
    // update status variable
    this.single_status_values = value;
    for (let i = 0; i < n_features; ++i) {
      this.single_status[i].checked = value[i];
      this.toggleSingle(i, value[i]);
    }
  }

};
