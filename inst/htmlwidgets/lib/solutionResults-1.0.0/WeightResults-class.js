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

    // TODO

  }

  /* render method */
  render() {
    return this.el;
  }

};
