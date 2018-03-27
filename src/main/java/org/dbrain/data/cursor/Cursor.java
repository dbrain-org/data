package org.dbrain.data.cursor;

/**
 * Bi-directional cursor contract.
 */
public interface Cursor extends ForwardCursor {

  boolean previous();

  boolean moveBof();

  boolean moveEof();

}
