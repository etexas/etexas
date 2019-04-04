package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.List;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.ExecutionsPage;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Partial page class to hold elements common to all execution partial pages
 *
 * @author llaroussini
 */
public class ExecutionPartialPage extends ExecutionsPage {

    /**
     * Default constructor
     *
     * @param driver the web driver
     */
    public ExecutionPartialPage(WebDriver driver) {
        super(driver);
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath to the rows in tables
     */
    protected static final String ROWS_XPATH = ".//table";

    /**
     * Xpath suffix to column header cells
     */
    protected static final String COLUMN_HEADER_CELL_XPATH_SUFFIX = "']//ancestor::div[1]";

    /**
     * Xpath prefix to a specific cell in table (with known value)
     */
    protected static final String SPECIFIC_CELL_XPATH_PREFIX = ".//div[@class='x-grid-cell-inner '][text()='";

    /**
     * Xpath suffix to specific cell in table (with known value)
     */
    protected static final String SPECIFIC_CELL_XPATH_SUFFIX = "']";

    /**
     * Xpath prefix to a cell containing specific text in table
     */
    protected static final String CELL_CONTAINS_TEXT_XPATH_PREFIX = ".//div[@class='x-grid-cell-inner '][contains, (text()='";

    /**
     * Xpath suffix to a cell containing specific text in table
     */
    protected static final String CELL_CONTAINS_XPATH_SUFFIX = "')]";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets list of displayed rows
     *
     * @param table -the table element to get the rows from
     * @return the list of rows
     */
    protected List<El> getRows(El table) {
        return table.els(By.xpath(ROWS_XPATH));
    }

    /**
     * Gets cell with given info in the given row
     *
     * @param row -the row where info is expected
     * @param info -the info expected
     * @return the cell element
     */
    protected El getInfoCell(El row, String info) {
        return row.el(By.xpath(SPECIFIC_CELL_XPATH_PREFIX + info + SPECIFIC_CELL_XPATH_SUFFIX));
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks to see if rows are displayed
     *
     * @param table -the table where rows are expected
     * @return true if displayed, false if not
     */
    public boolean areRowsDisplayed(El table) {
        try {
            return !(getRows(table).isEmpty());
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

}
