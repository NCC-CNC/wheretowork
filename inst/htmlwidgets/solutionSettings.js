// define HTML widgets interface
HTMLWidgets.widget({
  name: "solutionSettings",
  type: "output",
  factory: function(el, width, height) {
    // shared variables
    const elementId = el.id;
    let initialized = false;
    let container = document.getElementById(elementId);
    let handle = undefined;

    return {

      renderValue: function(opts) {
        // alias this
        let that = this;

        // initialize widget
        if (!initialized) {
          // set state to initialized
          initialized = true;
          // attach the widget to the DOM
          container.widget = that;
          // initialize solution settings manager
          handle = new SolutionSettings(
            elementId, container,
            opts.themes, opts.weights, opts.includes, opts.excludes, opts.parameters);
          // render HTML elements
          handle.render();
          // set the start and stop buttons are disabled by default
          const start_button_el =
            document.getElementById(elementId + "_start_button");
          const stop_button_el =
            document.getElementById(elementId + "_stop_button");
          start_button_el.setAttribute("disabled", "");
          stop_button_el.setAttribute("disabled", "");
          // make the button disabled when the text box is empty
          const name_el = document.getElementById(elementId + "_name");
          name_el.addEventListener("input", function() {
            if (name_el.value.length === 0) {
              start_button_el.setAttribute("disabled", "");
            } else {
              start_button_el.removeAttribute("disabled");
            }
          });
          name_el.setAttribute("maxlength", "50");
          // remove button tooltips are removed on click
          start_button_el.addEventListener("click", function(e) {
            $(".tooltip.fade.top.in").tooltip("hide");
          });
          stop_button_el.addEventListener("click", function(e) {
            $(".tooltip.fade.top.in").tooltip("hide");
          });
          // add color picker tooltip
          $("#" + elementId + "_color").tooltip({
            title: "Select a color for the new solution",
            trigger: "hover",
            placement: "top",
            container: "body"
          });
        }
      },

      resize: function(width, height) {
        // widget automatically resizes
      },

      // export object for extensibility
      solutionSettings: container,

      /* API functions to manipulate widget */
      update: function(params) {
        handle.updateSetting(
          params.value.id, params.value.setting,
          params.value.value, params.value.type);
      }

    };
  }
});

// Attach message handlers if in Shiny mode (these correspond to API)
if (HTMLWidgets.shinyMode) {
  const fxns = ["update"];

  let addShinyHandler = function(fxn) {
    return function() {
      Shiny.addCustomMessageHandler(
        "solutionSettings:" + fxn, function(message) {
          let el = document.getElementById(message.id);
          if (el) {
            delete message["id"];
            el.widget[fxn](message);
          }
        }
      );
    }
  };

  for (let i = 0; i < fxns.length; ++i) {
    addShinyHandler(fxns[i])();
  }
}
