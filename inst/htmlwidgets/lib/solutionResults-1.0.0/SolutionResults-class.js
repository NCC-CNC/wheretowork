class SolutionResults {
  /* constructor */
  constructor(id, container) {
    // set fields
    this.id = id,
    this.solutions = new Array();
    this.el = container;
    this.current_idx = undefined;

    // set fields to store HTML elements
    this.select_el =
      this.el.querySelector(".solution-select");
    this.result_container_el =
      this.el.querySelector(".solution-result-container");

    // alias this
    const that = this;

    // add event handle to select input
    this.select_el.addEventListener("change", function (e) {
      // suppress default form behavior
      e.preventDefault();
      // find id for selected solution
      const id = this[this.selectedIndex].id;
      // show selected solution
      that.showSolution(id);
    });
  }

  /* update methods */
  addSolution(solution) {
    // store solution
    this.solutions.push(solution);
    // create option HTML node for the new solution
    const option = document.createElement("option");
    option.innerText = solution.name;
    option.id = solution.id;
    // add the option to the select input
    this.select_el.appendChild(option);
  }

  dropSolution(id) {
    // find solution index
    const idx = this.solutions.findIndex((x) => x.id === id);
    // remove solution from widget
    if (idx > -1) {
      // if the solution is currently being shown,
      // then show a different solution before deleting it
      if (this.select_el.value === this.select_el.children[idx].innerText) {
        if (idx === 0) {
          // if deleting the first solution, then show the second one
          ths.showSolution(this.solutions[1].id);
        } else {
          // if deleting a different solution, then show the previous one
          this.showSolution(this.solutions[idx - 1].id);
        }
      }
      // remove solution from internal memory
      this.solutions.splice(idx, 1);
      // remove solution from select input
      this.select_el.removeChild(this.select_el.children[idx]);
    }
  }

  showSolution(id) {
    // find solution index
    const idx = this.solutions.findIndex((x) => x.id === id);
    // if solution found, then show it in the widget
    if (idx > -1) {
      // destroy charts if currently showing a solution
      if (typeof this.current_idx !== "undefined") {
        this.solutions[this.current_idx].destroy();
      }
      // clear solution from solution results container
      removeAllChildNodes(this.result_container_el);
      // add new solution to solution results container
      this.solutions[idx].render(this.result_container_el);
      // update select input
      this.select_el.value = this.select_el.children[idx].innerText;
      // update index of currently shown solution
      this.current_idx = idx;
    }
  }

  /* render method */
  render() {
    // none needed
  }

}
