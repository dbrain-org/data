package org.dbrain.data.lang;

import org.dbrain.data.Fqn;

public class Field extends Expression {

  private Fqn name;

  public Field(Fqn name) {
    this.name = name;
  }

  public Fqn getName() {
    return name;
  }
  
}
