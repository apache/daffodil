package org.apache.daffodil.layers.runtime1;

public enum LineFoldMode {
    /**
     * IMF - Internet Mail Format
     */
    IMF("lineFolded_IMF"),
    /**
     * iCalendar
     */
    iCalendar("lineFolded_iCalendar");

    public final String dfdlName;
    private LineFoldMode(final String dfdlName) { this.dfdlName = dfdlName; }

}
