class WeightResults {
  /* constructor */
  constructor(
    manager,
    id,
    name,
    status,
    total,
    factor,
    held,
    units
  ) {
    // class fields
    this.id = id;
    this.el =
      document.importNode(
        document
        .getElementById(manager)
        .querySelector(".weight-results-template")
        .content,
      true);

    // TODO: add code to display results for the object
    // The current code just inserts a TODO into the DOM
    const l = document.createElement("label");
    l.innerText = "TODO";
    this.el.appendChild(l);

  }

  /* render method */
  render() {
    return this.el;
  }

};
