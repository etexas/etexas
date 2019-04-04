package com.harmonia.qa.ETEXASWebQATests.webdriver.pages;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import org.apache.commons.io.FileUtils;
import org.openqa.selenium.By;
import org.openqa.selenium.JavascriptExecutor;
import org.openqa.selenium.Keys;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.interactions.Actions;
import org.openqa.selenium.support.ui.ExpectedCondition;
import org.openqa.selenium.support.ui.ExpectedConditions;

import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.webdriver.pages.base.PageBase;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * eTexas Base Page class
 *
 * @author cbulloss
 */
public class ETexasBasePage extends PageBase {

    /**
     * Constructor that ensures the AJAX monitoring javascript has been
     * injected.
     *
     * @param driver the web driver instance being used
     */
    public ETexasBasePage(WebDriver driver) {
        super(driver);
        JavascriptExecutor js = (JavascriptExecutor)driver;
        js.executeScript(ETEXAS_AJAX_JS);
        waitUntilLoaded();
    }

    /**
     * Static random value used in class methods
     */
    private static Random r = new Random();

    ///////////
    //ID's & Locators
    ///////////

    /**
     * Generic xpath prefix to an error tool tip
     */
    protected static final String GENERIC_ERROR_TOOLTIP_XPATH_PREFIX = "//div[contains(@class, 'x-form-invalid-icon')]/ancestor::div//span[contains(text(),'";

    /**
     * Xpath prefix to specific error tool tip
     */
    protected static final String SPECIFIC_ERROR_TOOLTIP_XPATH_PREFIX = "//div[contains(@data-errorqtip, '";

    /**
     * Xpath to error tool tip displayed when a field is required
     */
    protected static final String ERROR_TOOLTIP_REQUIRED_FIELD_XPATH_PREFIX = "//div[contains(@data-errorqtip, 'is required')]";

    /**
     * Xpath to error tool tip displayed when a non-numeric value is entered
     * into Speed field
     */
    protected static final String ERROR_TOOLTIP_INVALID_NUMBER_FIELD_XPATH = "//div[contains(@data-errorqtip, ' is not a valid number.')]";

    /**
     * Xpath to error tool tip displayed when the value is less than acceptable
     * minimum
     */
    protected static final String ERROR_TOOLTIP_MINIMUM_NUMBER_FIELD_XPATH = "//div[contains(@data-errorqtip, 'The minimum value for this field is')]";

    /**
     * Xpath to error tool tip displayed when the value is greater than
     * acceptable maximum Xpath to error tool tip displayed when a value greater
     * than the maximum is entered
     */
    protected static final String ERROR_TOOLTIP_MAXIMUM_NUMBER_FIELD_XPATH = "//div[contains(@data-errorqtip, 'The maximum value for this field is')]";

    /**
     * Xpath error for an invalid name field
     */
    protected static final String ERROR_TOOLTIP_INVALID_NAME_FIELD_XPATH = "//div[contains(@data-errorqtip, 'names may contain only letters, digits, hyphens, underscores, and nonconsecutive spaces.')]/ancestor::div//span[contains(text(),'";

    /**
     * Xpath error for an invalid IP address field
     */
    protected static final String ERROR_TOOLTIP_INVALID_HOST_ADDRESS_XPATH = "//div[contains(@data-errorqtip, 'is not a recognized IP address.')]";

    /**
     * Xpath prefix to a specific field name with invalid number error present
     */
    protected static final String SPECIFIC_INVALID_NUMBER_ERROR_XPATH_PREFIX = ERROR_TOOLTIP_INVALID_NUMBER_FIELD_XPATH + "/ancestor::div//span[contains(text(),'";

    /**
     * Xpath to error tool tip displayed when a field has leading/trailing
     * whitespace
     */
    protected static final String ERROR_TOOLTIP_WHITESPACE_XPATH = "//div[contains(@data-errorqtip, ' may not contain leading or trailing spaces.')]";

    /**
     * Xpath prefix to a specific field name with field required error present
     */
    protected static final String SPECIFIC_REQUIRED_FIELD_ERROR_XPATH_PREFIX = ERROR_TOOLTIP_REQUIRED_FIELD_XPATH_PREFIX + "/ancestor::div//span[contains(text(),'";

    /**
     * Xpath prefix to a specific field name with leading/trailing whitespace
     * error present
     */
    protected static final String SPECIFIC_WHITESPACE_ERROR_XPATH_PREFIX = ERROR_TOOLTIP_WHITESPACE_XPATH + "/ancestor::div//span[contains(text(),'";

    /**
     * Xpath prefix to a specific field name with minimum number required error
     * present
     */
    protected static final String SPECIFIC_MINIMUM_NUMBER_ERROR_XPATH_PREFIX = ERROR_TOOLTIP_MINIMUM_NUMBER_FIELD_XPATH + "/ancestor::div//span[contains(text(),'";

    /**
     * Xpath prefix to a specific field name with maximum number required error
     * present
     */
    protected static final String SPECIFIC_MAXIMUM_NUMBER_ERROR_XPATH_PREFIX = ERROR_TOOLTIP_MAXIMUM_NUMBER_FIELD_XPATH + "/ancestor::div//span[contains(text(),'";

    /**
     * Error text prefix when duplicate device name is used
     */
    protected static final String DUPLICATE_DEVICE_NAME_ERROR_TEXT_PREFIX = "A device or device profile with the name \"";

    /**
     * Error text suffix when duplicate device name is used
     */
    protected static final String DUPLICATE_DEVICE_NAME_ERROR_TEXT_SUFFIX = "\" already exists in the composite.";

    /**
     * Xpath suffix to a specific field name with error present
     */
    protected static final String FIELD_ERROR_XPATH_SUFFIX = "')]/ancestor::div[contains(@class, 'x-field')]//input";

    /**
     * Xpath prefix assigned to table cells
     */
    protected static final String CELL_XPATH_PREFIX = "//div[contains(@class, 'x-grid-cell-inner ')][text()='";

    /**
     * Xpath suffix for parent row of a cell in table
     */
    protected static final String PARENT_ROW_XPATH_SUFFIX = "/ancestor::tr";

    /**
     * Xpath of check box associated with simulations in list
     */
    protected static final String CHECK_BOX_XPATH = ".//div[@class='x-grid-row-checker']";

    /**
     * Xpath of row associated with a check box element
     */
    protected static final String CHECK_BOX_ROW_XPATH = ".//ancestor::table[contains(@id,'gridview')]";

    /**
     * Class of a row with a selected check box
     */
    protected static final String SELECTED_CHECK_BOX_ROW_CLASS = "x-grid-item-selected";

    /**
     * The contents of the javascript file used to initialize monitoring of AJAX
     * requests.
     */
    public static final String ETEXAS_AJAX_JS;

    static { //static initializer to store the js file's contents in a constant for injection
        String js = "";
        try {
            js = FileUtils.readFileToString(new File("xmlhttprequest.js")).replaceAll("//.*", "");//strip comments
        }
        catch (IOException e) {
            throw new RuntimeException("IO exception when loading AJAX javascript file.", e);
        }
        ETEXAS_AJAX_JS = js;
    }

    //////////
    // Getters
    //////////

    /**
     * Gets the field displaying 'field required' error
     *
     * @param fieldName -the name of the field expected to have error
     * @return the field with the given name with 'field required' error
     */
    protected El getGenericErrorTooltip(String fieldName) {
        return el(By.xpath(GENERIC_ERROR_TOOLTIP_XPATH_PREFIX + fieldName + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Gets the error tooltip displayed for given error
     *
     * @param text the error message expected associated with icon/tooltip
     * @return the error icon/tooltip
     */
    protected El getSpecificErrorToolTip(String text) {
        return el(By.xpath(SPECIFIC_ERROR_TOOLTIP_XPATH_PREFIX + text + "')]"));
    }

    //TODO Delete this method once the updated method has been replaced in all tests
    /**
     * Gets the field displaying 'field required' error
     *
     * @param fieldName -the name of the field expected to have error
     * @return the field with the given name with 'field required' error
     */
    protected El getFieldErrorRequiredField(String fieldName) {
        return el(By.xpath(SPECIFIC_REQUIRED_FIELD_ERROR_XPATH_PREFIX + fieldName + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Gets the field is required error
     *
     * @return the El with the error tooltip
     */
    protected El getFieldErrorRequiredXpath() {
        return el(By.xpath(ERROR_TOOLTIP_REQUIRED_FIELD_XPATH_PREFIX));
    }

    //TODO Delete this method once the updated method has been replaced in all tests
    /**
     * Gets the field displaying leading/trailing whitespace error
     *
     * @param fieldName -the name of the field expected to have error
     * @return the field with the given name with leading/trailing whitespace
     *         error
     */
    protected El getFieldErrorWhitespace(String fieldName) {
        return el(By.xpath(SPECIFIC_WHITESPACE_ERROR_XPATH_PREFIX + fieldName + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Gets the field may not contain leading or trailing spaces error
     *
     * @return the El with the error tooltip
     */
    protected El getFieldErrorWhitespaceXpath() {
        return el(By.xpath(ERROR_TOOLTIP_WHITESPACE_XPATH));
    }

    //TODO Delete this method once the updated method has been replaced in all tests
    /**
     * Gets the field displaying invalid number error
     *
     * @param fieldName -the name of the field expected to have error
     * @return the field with the given name with invalid number error
     */
    protected El getFieldErrorInvalidNumber(String fieldName) {
        return el(By.xpath(SPECIFIC_INVALID_NUMBER_ERROR_XPATH_PREFIX + fieldName + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Gets the field may not contain leading or trailing spaces error
     *
     * @return the El with the error tooltip
     */
    protected El getFieldErrorInvalidNumberXpath() {
        return el(By.xpath(ERROR_TOOLTIP_INVALID_NUMBER_FIELD_XPATH));
    }

    //TODO Delete this method once the updated method has been replaced in all tests
    /**
     * Gets the field displaying minimum number error
     *
     * @param fieldName -the name of the field expected to have error
     * @return the field with the given name with minimum number error
     */
    protected El getFieldErrorMinimumNumber(String fieldName) {
        return el(By.xpath(SPECIFIC_MINIMUM_NUMBER_ERROR_XPATH_PREFIX + fieldName + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Gets the field required minimum number spaces error
     *
     * @return the El with the error tooltip
     */
    protected El getFieldErrorMinimumNumberXpath() {
        return el(By.xpath(ERROR_TOOLTIP_MINIMUM_NUMBER_FIELD_XPATH));
    }

    //TODO Delete this method once the updated method has been replaced in all tests
    /**
     * Gets the field displaying maximum number error
     *
     * @param fieldName -the name of the field expected to have error
     * @return the field with the given name with maximum number error
     */
    protected El getFieldErrorMaximumNumber(String fieldName) {
        return el(By.xpath(SPECIFIC_MAXIMUM_NUMBER_ERROR_XPATH_PREFIX + fieldName + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Gets the field required maximum number spaces error
     *
     * @return the El with the error tooltip
     */
    protected El getFieldErrorMaximumNumberXpath() {
        return el(By.xpath(ERROR_TOOLTIP_MAXIMUM_NUMBER_FIELD_XPATH));
    }

    //TODO Delete this method once the updated method has been replaced in all tests
    /**
     * Gets the field displaying invalid field name error
     *
     * @param fieldName -the name of the field expected to have error
     * @return the field with the given name with maximum number error
     */
    protected El getFieldErrorInvalidName(String fieldName) {
        return el(By.xpath(ERROR_TOOLTIP_INVALID_NAME_FIELD_XPATH + fieldName + FIELD_ERROR_XPATH_SUFFIX));
    }

    /**
     * Gets the field invalid name error
     *
     * @return the El with the error tooltip
     */
    protected El getFieldErrorInvalidNameXpath() {
        return el(By.xpath(ERROR_TOOLTIP_INVALID_NAME_FIELD_XPATH));
    }

    /**
     * Gets cell in table with given value //This will only work with UNIQUE
     * values
     *
     * @param value -the value expected
     * @return the cell element
     */
    protected By getCell(String value) {
        return By.xpath(CELL_XPATH_PREFIX + value + "']");
    }

    /**
     * Gets the row from a cell with the given value //must use a UNIQUE value
     *
     * @param value -value expected in cell of the row
     * @return the row element
     */
    protected El getRow(String value) {
        String cellXpath = CELL_XPATH_PREFIX + value + "']";
        return el(By.xpath(cellXpath + PARENT_ROW_XPATH_SUFFIX));
    }

    /**
     * Gets the checkbox associated with the given device name
     *
     * @param attribute -the UNIQUE attribute in a row for which the checkbox
     *        should be retrieved
     * @return the checkbox element
     */
    protected El getCheckbox(String attribute) {
        El row = getRow(attribute);
        return row.el(By.xpath(CHECK_BOX_XPATH));
    }

    /**
     * Gets a random value between the two given values
     *
     * @param lowerBound -the lower bound of the range from which to get a
     *        random int (inclusive)
     * @param upperBound -the upper bound of the range from which to get a
     *        random int (exclusive)
     * @return a random int within the given range
     */
    public int getRandomValue(int lowerBound, int upperBound) { //TODO add to automation base RandomNumberGenerator
        return r.nextInt(upperBound - lowerBound) + lowerBound;
    }

    ////////////
    // Checkers
    ////////////

    /**
     * Checks if given error tooltip is displayed
     *
     * @param text the error message expected associated with icon/tooltip
     * @return true if displayed, false otherwise
     */
    protected boolean isErrorToolTipDisplayed(String text) {
        return isElementDisplayed(getSpecificErrorToolTip(text));
    }

    //TODO Delete this method once all tests have been updated
    /**
     * Checks if field required error is displayed for the specified field name
     *
     * @param fieldName -the name of the field where 'field required' error is
     *        expected
     * @return true if displayed, false otherwise
     */
    protected boolean isFieldRequiredErrorDisplayed(String fieldName) {
        return isElementDisplayed(getFieldErrorRequiredField(fieldName));
    }

    /**
     * Checks if field required error is displayed
     *
     * @return true if displayed, false otherwise
     */
    protected boolean isFieldRequiredErrorDisplayed() {
        return isElementDisplayed(getFieldErrorRequiredXpath());
    }

    //TODO Delete this method once all tests have been updated
    /**
     * Checks if leading/trailing whitespace error is displayed for the
     * specified field name
     *
     * @param fieldName -the name of the field where leading/trailing whitespace
     *        error is expected
     * @return true if displayed, false otherwise
     */
    protected boolean isWhitespaceErrorDisplayed(String fieldName) {
        return isElementDisplayed(getFieldErrorWhitespace(fieldName));
    }

    /**
     * Checks if leading or trailing spaces error is displayed
     *
     * @return true if displayed, false otherwise
     */
    protected boolean isWhitespaceErrorDisplayed() {
        return isElementDisplayed(getFieldErrorWhitespaceXpath());
    }

    //TODO Delete this method once all tests are updated
    /**
     * Checks if invalid number error is displayed for the specified field name
     *
     * @param fieldName -the name of the field where invalid number error is
     *        expected
     * @return true if displayed, false otherwise
     */
    protected boolean isInvalidNumberErrorDisplayed(String fieldName) {
        return isElementDisplayed(getFieldErrorInvalidNumber(fieldName));
    }

    /**
     * Checks if invalid number error is displayed
     *
     * @return true if displayed, false otherwise
     */
    protected boolean isInvalidNumberErrorDisplayed() {
        return isElementDisplayed(getFieldErrorInvalidNumberXpath());
    }

    //TODO Delete this method once all tests are updated
    /**
     * Checks if minimum number error is displayed for the specified field name
     *
     * @param fieldName -the name of the field where minimum number error is
     *        expected
     * @return true if displayed, false otherwise
     */
    protected boolean isMinimumNumberErrorDisplayed(String fieldName) {
        return isElementDisplayed(getFieldErrorMinimumNumber(fieldName));
    }

    /**
     * Checks if minimum number error is displayed
     *
     * @return true if displayed, false otherwise
     */
    protected boolean isMinimumNumberErrorDisplayed() {
        return isElementDisplayed(getFieldErrorMinimumNumberXpath());
    }

    //TODO Delete this method once all tests have been updated
    /**
     * Checks if maximum number error is displayed for the specified field name
     *
     * @param fieldName -the name of the field where minimum number error is
     *        expected
     * @return true if displayed, false otherwise
     */
    protected boolean isMaxmimumNumberErrorDisplayed(String fieldName) {
        return isElementDisplayed(getFieldErrorMaximumNumber(fieldName));
    }

    /**
     * Checks if maximum number error is displayed
     *
     * @return true if displayed, false otherwise
     */
    protected boolean isMaxmimumNumberErrorDisplayed() {
        return isElementDisplayed(getFieldErrorMaximumNumberXpath());
    }

    //TODO Delete this method once all tests are updated
    /**
     * Checks if invalid name error is displayed for the specified field name
     *
     * @param fieldName -the name of the field where minimum number error is
     *        expected
     * @return true if displayed, false otherwise
     */
    protected boolean isInvalidNameErrorDisplayed(String fieldName) {
        return isElementDisplayed(getFieldErrorInvalidName(fieldName));
    }

    /**
     * Checks if invalid name error is displayed
     *
     * @return true if displayed, false otherwise
     */
    protected boolean isInvalidNameErrorDisplayed() {
        return isElementDisplayed(getFieldErrorInvalidNameXpath());
    }

    /**
     * Checks if duplicate device error is displayed
     *
     * @param name -the name of the duplicated device
     * @return true if displayed, false otherwise
     */
    protected boolean isDuplicateDeviceNameErrorDisplayed(String name) {
        return isErrorToolTipDisplayed(DUPLICATE_DEVICE_NAME_ERROR_TEXT_PREFIX + name + DUPLICATE_DEVICE_NAME_ERROR_TEXT_SUFFIX);
    }

    /**
     * Checks if pagination button is enabled
     *
     * @param by -the by of the button element
     * @return true if enabled, false otherwise
     */
    public boolean isPaginationButtonEnabled(By by) {
        return el(by).isEnabled();
    }

    /**
     * Checks if row containing given attribute is selected
     *
     * @param attribute -the attribute associated with the row to check
     * @return true if selected, false if de-selected
     */
    protected boolean isRowSelected(String attribute) {
        El row = getRow(attribute);
        String rowState = row.getAttribute("aria-selected");
        if (rowState.equals("true")) {
            return true;
        }
        else {
            return false;
        }
    }

    //////////////
    // Utilities
    //////////////

    /**
     * Waits for the page to finish loading
     */
    public void waitUntilLoaded() {
        long timeBefore = System.currentTimeMillis();
        newWait(5).until(new ExpectedCondition<Boolean>() {

            @Override
            public Boolean apply(WebDriver d) {
                String script = "return (document.readyState === 'complete');";
                return (Boolean)((JavascriptExecutor)d).executeScript(script);
            }
        });
        logSleepWait(System.currentTimeMillis() - timeBefore);
    }

    /**
     * Waits up to the specified timeout for an element located by a specified
     * locator is visible
     *
     * @param by the locator suitable for locating the expected element
     * @param timeout the amount of time to wait
     */
    protected void waitUntilAlertIsPresent(By by, int timeout) {
        long before = System.currentTimeMillis();
        driverWait(timeout).until(ExpectedConditions.visibilityOfElementLocated(by));
        long after = System.currentTimeMillis();
        logSleepWait(after - before);
    }

    /**
     * Waits a specified amount of time until the alert dialog is no longer
     * displayed
     *
     * @param by -By of the element expected to disappear
     * @param timeout the amount of time in seconds to wait for the alert to
     *        disappear.
     */
    protected void waitUntilAlertIsNotPresent(By by, int timeout) {
        long before = System.currentTimeMillis();
        driverWait(timeout).until(ExpectedConditions.invisibilityOfElementLocated(by));
        long after = System.currentTimeMillis();
        logSleepWait(after - before);
    }

    /**
     * Finds the row in table with the given text
     *
     * @param text - text expected in the table
     * @param table - the table element to search
     * @param rowXpath -the xpath to rows in the given table
     * @return the row in which the given applicant is displayed This method is
     *         a slight variation on the PageBase method as tables in the etexas
     *         UI utilize different xpaths for rows in different tables
     */
    protected El rowInTable(String text, El table, String rowXpath) {
        List<El> rows = table.els(By.xpath(rowXpath));
        El theRow = null;

        Iterator<El> iterator = rows.iterator();
        while (iterator.hasNext() && theRow == null) {
            El row = iterator.next();

            List<El> cells = row.els(By.xpath(".//td"));
            for (El cell : cells) {
                String cellValue = cell.getText();
                if (cellValue.contains(text)) {
                    theRow = row;
                    break;

                }
            }

        }

        return theRow;
    }

    /**
     * Finds list of rows in the given table with the given value
     *
     * @param text - text displayed in multiple rows
     * @param table - the table element to search
     * @return list of rows is which the text is displayed
     */
    public List<El> rowsInTable(String text, El table) {
        List<El> rows = table.els(By.xpath(".//table"));
        List<El> selectRows = new ArrayList<El>();

        Iterator<El> iterator = rows.iterator();
        while (iterator.hasNext()) {
            El row = iterator.next();

            List<El> cells = row.els(By.xpath(".//td"));
            for (El cell : cells) {
                String cellValue = cell.getText();
                if (cellValue.contains(text)) {
                    selectRows.add(row);
                }
            }
        }
        return selectRows;
    }

    /**
     * Finds the row in table with the multiple values given
     *
     * @param rows - list of rows that contain a common value
     * @param uniqueValue - text that is unique to the rows
     * @param table - the table element to search
     * @return list of rows is which the first value is displayed
     */
    public El rowInTableMultipleValues(List<El> rows, String uniqueValue, El table) {
        waitUntilLoaded();
        El theRow = null;
        List<El> newRows = rows;

        Iterator<El> newIterator = newRows.iterator();
        while (newIterator.hasNext()) {
            El newRow = newIterator.next();

            List<El> newCells = newRow.els(By.xpath(".//td"));
            for (El newCell : newCells) {
                String cellText = newCell.getText();
                if (cellText.contains(uniqueValue)) {
                    theRow = newRow;
                    break;
                }
            }
        }
        return theRow;

    }

    /**
     * Finds the row in table with the multiple values given
     *
     * @param commonValue - value that is common to all rows to be searched
     * @param uniqueValue - text that is unique to the rows
     * @param table - the table element to search
     * @return list of rows is which the first value is displayed
     */
    public El rowInTableMultipleValues(String commonValue, String uniqueValue, El table) {
        List<El> rows = rowsInTable(commonValue, table);
        waitUntilLoaded();
        El theRow = null;
        List<El> newRows = rows;

        Iterator<El> newIterator = newRows.iterator();
        while (newIterator.hasNext() && theRow == null) {
            El newRow = newIterator.next();

            List<El> newCells = newRow.els(By.xpath(".//td"));
            for (El newCell : newCells) {
                String cellText = newCell.getText();
                if (cellText.contains(uniqueValue)) {
                    theRow = newRow;
                    break;
                }
            }
        }
        return theRow;

    }

    /**
     * Double clicks and copies the element given to be copied, then pastes the
     * element into the given text box el
     *
     * @param copyEl -the element to be copied
     * @param pasteEl -the text box element where text should be pasted
     */
    protected void selectCopyAndPaste(El copyEl, El pasteEl) { //TODO remove when added to base-automation and release is made
        Actions action = new Actions(driver);
        action.doubleClick(copyEl.webElement()).sendKeys(Keys.chord(Keys.CONTROL, "c")).perform();
        pasteEl.setText((Keys.chord(Keys.CONTROL, "v")));
    }

    /**
     * Selects row based on given attribute and boolean value
     *
     * @param attribute -the name of any UNIQUE attribute displayed in the row
     *        for to select
     * @param selected -true if row should be selected, false if not
     */
    public void selectRow(String attribute, boolean selected) {
        El row = getRow(attribute);
        String rowState = row.getAttribute("aria-selected");
        boolean isSelected = rowState != null;
        waitUntilLoaded();
        if (selected != isSelected) {
            row.click();
            ETexasCommonUtils.sleep(1000); //ZZZ - time to ensure buttons are enabled/disabled following selection
        }
        ETexasCommonUtils.sleep(1000); //ZZZ - allows time for buttons to become enabled/disabled following selection
    }

    /**
     * Copies and pastes given value into given field
     *
     * @param copyEl -the element that should be copied
     * @param pasteEl -the element in which the copied value should be pasted
     */
    public void copyPaste(El copyEl, El pasteEl) {
        copyEl.sendKeys(Keys.CONTROL + "a");
        copyEl.sendKeys(Keys.CONTROL + "c");
        pasteEl.click();
        pasteEl.sendKeys(Keys.CONTROL + "v");
    }

    /**
     * Waits for the total number of all monitored open AJAX requests to reach 0
     * twice in a row.
     */
    public void waitForEtexasAjaxCompletion() {
        long before = System.currentTimeMillis();
        newWait(AJAX_COMPLETION_TIMEOUT).pollingEvery(AJAX_POLLING_INTERVAL, TimeUnit.MILLISECONDS).until(new ExpectedCondition<Boolean>() {

            private Long last = -1L;

            @Override
            public Boolean apply(WebDriver d) {
                Long total = (Long)((JavascriptExecutor)d).executeScript("return sqaAjax.total;");
                if (total == 0 && last == 0) {
                    return true;
                }
                else {
                    last = total;
                    return false;
                }
            }
        });
        long after = System.currentTimeMillis();
        logSleepWait(after - before);
    }

    /**
     * Waits for the total number of monitored open AJAX requests with URL
     * endpoints and HTTP methods matching the specified regular expression and
     * method to reach 0 twice in a row. Note that the method is
     * case-insensitive.
     * <p>
     * For example, passing a string like "/\\/simulations\\/templates" would
     * match any request made to the templates endpoint. Note that the forward
     * slashes before "simulations" and "templates" are double-escaped: once for
     * javascript, then again for java. The actual string sent to the javascript
     * method will look like "/\/simulations\/templates".
     * </p>
     * TODO look into simplifying regex argument
     *
     * @param regex the regular expression to match URL endpoints of AJAX
     *        requests against
     * @param method the HTTP method (e.g. GET, POST, PUT, etc.) matching the
     *        method used by the AJAX request
     */
    public void waitForEtexasAjaxCompletion(final String regex, final String method) {
        long before = System.currentTimeMillis();
        newWait(AJAX_COMPLETION_TIMEOUT).pollingEvery(AJAX_POLLING_INTERVAL, TimeUnit.MILLISECONDS).until(new ExpectedCondition<Boolean>() {

            private Long last = -1L;

            @Override
            public Boolean apply(WebDriver d) {
                Long total = (Long)((JavascriptExecutor)d).executeScript("return sqaAjax.matchedOpen(" + regex + ", \"" + method + "\");");
                if (total == 0 && last == 0) {
                    return true;
                }
                else {
                    last = total;
                    return false;
                }
            }
        });
        long after = System.currentTimeMillis();
        logSleepWait(after - before);
    }
}
