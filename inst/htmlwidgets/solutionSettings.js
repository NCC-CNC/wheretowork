// define HTML widgets interface
HTMLWidgets.widget({
  name: "solutionSettings",
  type: "output",
  factory: function(el, width, height) {
    // shared variables
    var elementId = el.id;
    var initialized = false;
    var container = document.getElementById(elementId);
    var handle = undefined;

    return {

      renderValue: function(opts) {
        // alias this
        var that = this;

        // initialize widget
        if (!initialized) {
          // set state to initialized
          initialized = true;
          // attach the widget to the DOM
          container.widget = that;
          // initialize solution settings manager
          handle = new SolutionSettings(
            elementId, container,
            opts.themes, opts.weights, opts.includes, opts.parameters);
          // render HTML elements
          handle.render();
          // make the button disabled when the text box is empty
          const button_el = document.getElementById(elementId + "_button");
          const name_el = document.getElementById(elementId + "_name");
          name_el.addEventListener("input", function() {
            if (name_el.value.length === 0) {
              button_el.setAttribute("disabled", "");
            } else {
              button_el.removeAttribute("disabled");
            }
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
  var fxns = ["update"];

  var addShinyHandler = function(fxn) {
    return function() {
      Shiny.addCustomMessageHandler(
        "solutionSettings:" + fxn, function(message) {
          var el = document.getElementById(message.id);
          if (el) {
            delete message["id"];
            el.widget[fxn](message);
          }
        }
      );
    }
  };

  for (var i = 0; i < fxns.length; ++i) {
    addShinyHandler(fxns[i])();
  }
}
