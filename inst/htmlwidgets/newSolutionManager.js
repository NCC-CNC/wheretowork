// define HTML widgets interface
HTMLWidgets.widget({
  name: 'newSolutionManager',
  type: 'output',
  factory: function(el, width, height) {
    // top-level variables
    var elementId = el.id;
    var initialized = false;
    var container = document.getElementById(elementId);
    var handle = undefined;

    return {

      renderValue: function(opts) {

        if (!initialized) {
          // initialize manager
          handle = new NewSolutionManager(
            elementId, container, x.themes, x.weights);
          // render manager
          handle.render();

          // local variables
          let name_input = container.querySelector(".new-solution-name");
          let button = container.querySelector(".new-solution-button");

          // set listeners to update user interfance
          if (HTMLWidgets.shinyMode) {
            /// disable button if new input has no characters in it
            name_input.addEventListener("change", function () {
              if (this.value.length === 0) {
                button.setAttribute("disabled", "");
              } else {
                button.removeAttribute("disabled");
              }
            })
          }

          // set listeners to pass data to Shiny
          if (HTMLWidgets.shinyMode) {
            button.addEventListener("click", function() {
              Shiny.setInputValue(manager + "_run", name_input.value);
            });
          }

      },

      resize: function(width, height) {
        // widget automatically resizes
      }

      // export object for extensibility
      object: container,

      /* API functions to manipulate widget */
      // theme functions
      updateThemeName: function(params) {
        handle.updateThemeName(params.id, params.value);
      },

      updateThemeStatus: function(params) {
        handle.updateThemeStatus(params.id, params.value);
      },

      updateThemeView: function(params) {
        handle.updateThemeView(params.id, params.value);
      },

      updateThemeGroupGoal: function(params) {
        handle.updateThemeGroupGoal(params.id, params.value);
      },

      updateThemeFeatureGoals: function(params) {
        handle.updateThemeFeatureGoals(params.id, params.value);
      },

      updateThemeFeatureStatuses: function(params) {
        handle.updateThemeFeatureStatuses(params.id, params.value);
      },

      // weight functions
      updateWeightName: function(params) {
        handle.updateWeightName(params.id, params.value);
      },

      updateWeightStatus: function(params) {
        handle.updateWeightStatus(params.id, params.value);
      },

      updateWeightFactor: function(params) {
        handle.updateWeightFactor(params.id, params.value);
      }

    };
  }
});

// Attach message handlers if in Shiny mode (these correspond to API)
if (HTMLWidgets.shinyMode) {
  var fxns =
    ["updateThemeName", "updateThemeStatus", "updateThemeView",
     "updateThemeGroupGoal",
     "updateThemeFeatureGoals", "updateThemeFeatureStatuses",
     "updateWeightName", "updateWeightStatus", "updateWeightFactor"];

  var addShinyHandler = function(fxn) {
    return function() {
      Shiny.addCustomMessageHandler(
        "theme:goal:" + fxn, function(message) {
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
