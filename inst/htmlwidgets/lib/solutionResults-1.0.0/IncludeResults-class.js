class IncludeResults {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    status,
    total_amount,
    solution_held,
    units,
    provenance,
    solution_color
  ) {
    // declare fields
    this.id = id;

    // create HTML element
    this.el = document.createElement("div");
    this.el.classList.add("include-result");
    // create chart
    const chart = new IncludeSolutionChart([{
      name,
      solution_held,
      total_amount,
      units,
      status,
      total: 1,
    }], {
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
