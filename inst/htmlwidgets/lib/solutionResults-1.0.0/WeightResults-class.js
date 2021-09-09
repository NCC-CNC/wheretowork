class WeightResults {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    status,
    factor,
    total_amount,
    current_held,
    solution_held,
    units,
    provenance,
    solution_color
  ) {
    // declare fields
    this.id = id;

    // create HTML element
    this.el = document.createElement("div");
    this.el.classList.add("weight-result");

    // manually coerce factor to zero if status is false
    const manual_factor = status ? factor : 0;

    // create chart
    const chart = new WeightSolutionChart([{
      name,
      current_held,
      solution_held,
      total_amount,
      units,
      status,
      factor: manual_factor,
      total: 1,
    }], {
      current_held: "#06d6a0",
      solution_held: solution_color,
      total: "#cccccc44"
    });

    // render chart on HTML element
    chart.render(this.el);

    // add provenance
    createProvenance(
      this.el.querySelector(".provenance-container label"), provenance
    );
    $(this.el).find('[data-toggle="tooltip"]').tooltip()

  }

  /* render method */
  render() {
    return this.el;
  }

};
