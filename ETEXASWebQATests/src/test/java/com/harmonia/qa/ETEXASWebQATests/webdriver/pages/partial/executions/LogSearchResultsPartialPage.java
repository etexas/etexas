package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions;

import java.util.Iterator;
import java.util.List;

import org.junit.Assert;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.support.ui.ExpectedConditions;

import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * The partial page displaying search results when search is performed on
 * Exeuction Details page
 *
 * @author llaroussini
 */
public class LogSearchResultsPartialPage extends LogsPartialPage {

    /**
     * Default constructor
     *
     * @param driver -the web driver
     */
    public LogSearchResultsPartialPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Options for displayed entries per page
     *
     * @author llaroussini
     */
    public enum EntriesPerPage {
        /**
         * 10 entries per page
         */
        TEN("10"),
        /**
         * 25 entries per page
         */
        TWENTY_FIVE("25"),
        /**
         * 50 entries per page
         */
        FIFTY("50"),
        /**
         * 75 entries per page
         */
        SEVENTY_FIVE("75"),
        /**
         * 100 entries per page
         */
        ONE_HUNDRED("100");

        /**
         * The label of the entry per page options as they appear in the
         * application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        EntriesPerPage(String label) {
            this.label = label;
        }

        /**
         * Gets the label (displayed value) associated with the entries per page
         * option as it is displayed in the Web UI
         *
         * @return The label of the header cell
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * Search Criteria lists
     *
     * @author llaroussini
     */
    public enum SearchCriteriaListName {
        /**
         * Devices list name
         */
        DEVICES("Devices"),
        /**
         * Apps list name
         */
        APPS("Apps"),
        /**
         * Keys list name
         */
        KEYS("Keys");

        /**
         * The label/name of the search criteria lists as they appear in the
         * application
         */
        private String label;

        /**
         * Default constructor; sets the label/name
         *
         * @param label The string to set as the label/name
         */
        private SearchCriteriaListName(String label) {
            this.label = label;
        }

        /**
         * Gets the label (displayed value) associated with the search criteria
         * list as it is displayed in the Web UI
         *
         * @return The label of the list name
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////
    /**
     * The Xpath of the box displaying the current page of a table
     */
    private static final String PAGE_IN_TABLE_BOX_XPATH = "//div[contains(@id,'pagingtoolbar')]//div[contains(@id, 'numberfield')]//input";

    /**
     * The Xpath of the box displaying the total pages in a table
     */
    private static final String TOTAL_PAGES_BOX_XPATH = "//div[contains(@id, 'tbtext')][contains(text(),'of')]";

    /**
     * The Xpath of the link which navigates to the first page of a table
     */
    private static final String FIRST_PAGE_IN_TABLE_LINK_XPATH = "//span[contains(@class, 'x-tbar-page-first')]//ancestor::a";

    /**
     * The Xpath of the link which navigates to the previous page in a table
     */
    private static final String PREVIOUS_PAGE_IN_TABLE_LINK_XPATH = "//span[contains(@class, 'x-tbar-page-prev')]//ancestor::a";

    /**
     * The Xpath of the link which navigates to the next page in a table
     */
    private static final String NEXT_PAGE_IN_TABLE_LINK_XPATH = "//span[contains(@class, 'x-tbar-page-next')]//ancestor::a";

    /**
     * The Xpath of the link which navigates to the last page of a table
     */
    private static final String LAST_PAGE_IN_TABLE_LINK_XPATH = "//span[contains(@class, 'x-tbar-page-last')]//ancestor::a";

    /**
     * The xpath of the entries per page dropdown selector
     */
    private static final String ENTRIES_PER_PAGE_DROPDOWN_SELECTOR_XPATH = "//div[contains(@id, 'combobox')]//div[contains(@class, 'x-form-trigger-default')]";

    /**
     * The xpath for dropdown options
     */
    private static final String DROPDOWN_OPTIONS_XPATH = "//div[contains(@class, 'x-boundlist-default')]//li[@role='option']";

    /**
     * The xpath prefix for a specific option in the dropdown menu
     */
    private static final String DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX = DROPDOWN_OPTIONS_XPATH + "[text()='";

    /**
     * Xpath of the selected option in entries per page drop down
     */
    private static final String SELECTED_OPTION_XPATH = "//li[contains(@class, 'x-boundlist-selected')]";

    /**
     * Xpath of the search results table
     */
    private static final String SEARCH_RESULTS_TABLE_XPATH = "//div[contains(@id, 'appLogTable')]";

    /**
     * Xpath to be used following row element to find a specific cell in a row
     */
    private static final String CELL_IN_TABLE_XPATH_PREFIX = ".//div[text()='";

    /**
     * Xpath the Device column of a specific row
     */
    private static final String DEVICE_COLUMN_IN_ROW_XPATH = ".//td[1]/div"; //TODO update when unique identifier is in place

    /**
     * Xpath the App column of a specific row
     */
    private static final String APP_COLUMN_IN_ROW_XPATH = ".//td[2]/div"; //TODO update when unique identifier is in place

    /**
     * Xpath the Sim Time column of a specific row
     */
    private static final String SIM_TIME_COLUMN_IN_ROW_XPATH = ".//td[3]/div"; //TODO update when unique identifier is in place

    /**
     * Xpath the Key column of a specific row
     */
    private static final String KEY_COLUMN_IN_ROW_XPATH = ".//td[4]/div"; //TODO update when unique identifier is in place

    /**
     * Xpath the Data column of a specific row
     */
    private static final String DATA_COLUMN_IN_ROW_XPATH = ".//td[5]/div"; //TODO update when unique identifier is in place

    /**
     * Xpath prefix to lists in Search Criteria
     */
    private static final String LIST_XPATH_PREFIX = "//label[text()='";

    /**
     * Xpath suffix lists in Search Criteria
     */
    private static final String LIST_XPATH_SUFFIX = "']/ancestor::div[1]//div[@class='x-grid-item-container']";

    /**
     * Xpath prefix (to be used with list xpath) to row in list in Search
     * Criteria
     */
    private static final String ROW_IN_LIST_XPATH_PREFIX = ".//tr//div[text()='";

    /**
     * Xpath to selected option in Search Criteria list
     */
    private static final String SELECTED_OPTION_IN_LIST_XPATH = "//div[contains(@id, 'panel')]//table[contains(@class, 'x-grid-item-selected')]";

    /**
     * Xpath to search for when verifying search results
     */
    private static final String NO_SEARCH_RESULTS_XPATH = "//div[text()='No data to display']";

    /**
     * Displayed name of the Min Sim Time text box
     */
    private static final String MIN_SIM_TIME_FIELD_NAME = "Min:";

    ///////////
    //Element Getters
    ///////////
    /**
     * Gets the App Log Search results table
     *
     * @return the table element
     */
    private El getSearchResultsTable() {
        return el(By.xpath(SEARCH_RESULTS_TABLE_XPATH));
    }

    /**
     * Gets the App Log Search results rows
     *
     * @return the rows list element
     */
    private List<El> getSearchResultRows() {
        return getSearchResultsTable().els(By.xpath(".//tr"));
    }

    /**
     * Gets the first row from search results table
     *
     * @return the row element
     */
    private El getFirstSearchResultRow() {
        return getSearchResultsTable().el(By.xpath(".//tr[1]"));
    }

    /**
     * Gets the Device column cell within the specified row
     *
     * @param row -the row in which is column is desired
     * @return the Device column cell in the specified row
     */
    private El getDeviceColumnCell(El row) {
        return row.el(By.xpath(DEVICE_COLUMN_IN_ROW_XPATH));
    }

    /**
     * Gets the App column cell within the specified row
     *
     * @param row -the row in which is column is desired
     * @return the App column cell in the specified row
     */
    private El getAppColumnCell(El row) {
        return row.el(By.xpath(APP_COLUMN_IN_ROW_XPATH));
    }

    /**
     * Gets the Sim Time column cell within the specified row
     *
     * @param row -the row in which is column is desired
     * @return the Sim Time column cell in the specified row
     */
    private El getSimTimeColumnCell(El row) {
        return row.el(By.xpath(SIM_TIME_COLUMN_IN_ROW_XPATH));
    }

    /**
     * Gets the Key column cell within the specified row
     *
     * @param row -the row in which is column is desired
     * @return the Key column cell in the specified row
     */
    private El getKeyColumnCell(El row) {
        return row.el(By.xpath(KEY_COLUMN_IN_ROW_XPATH));
    }

    /**
     * Gets the Data column cell within the specified row
     *
     * @param row -the row in which is column is desired
     * @return the Data column cell in the specified row
     */
    private El getDataColumnCell(El row) {
        return row.el(By.xpath(DATA_COLUMN_IN_ROW_XPATH));
    }

    /**
     * Gets the text displayed in the Device column of a specified row
     *
     * @param row -the row where Device column text is expected
     * @return the text displayed in the Device column of the given row
     */
    private String getDisplayedDevice(El row) {
        return getDeviceColumnCell(row).getText();
    }

    /**
     * Gets the text displayed in the App column of a specified row
     *
     * @param row -the row where App column text is expected
     * @return the text displayed in the App column of the given row
     */
    private String getDisplayedApp(El row) {
        return getAppColumnCell(row).getText();
    }

    /**
     * Gets the text displayed in the Sim Time column of a specified row
     *
     * @param row -the row where Sim Time column text is expected
     * @return the text displayed in the Sim Time column of the given row
     */
    private String getDisplayedSimTime(El row) {
        return getSimTimeColumnCell(row).getText();
    }

    /**
     * Gets the text displayed in the Key column of a specified row
     *
     * @param row -the row where Key column text is expected
     * @return the text displayed in the Key column of the given row
     */
    private String getDisplayedKey(El row) {
        return getKeyColumnCell(row).getText();
    }

    /**
     * Gets the text displayed in the Data column of a specified row
     *
     * @param row -the row where Data column text is expected
     * @return the text displayed in the Data column of the given row
     */
    private String getDisplayedData(El row) {
        return getDataColumnCell(row).getText();
    }

    /**
     * Gets cell with given value in given row
     *
     * @param row -the row where cell is expected
     * @param value -the value expected to be displayed in row
     * @return the cell element
     */
    private El getCellInRow(El row, String value) {
        return row.el(By.xpath(CELL_IN_TABLE_XPATH_PREFIX + value + "']"));
    }

    /**
     * Gets the box displaying the total pages in a table
     *
     * @return the box element
     */
    private El getTotalPagesBox() {
        return el(By.xpath(TOTAL_PAGES_BOX_XPATH));
    }

    /**
     * Gets the box displaying the current page in a table
     *
     * @return the box element
     */
    private El getPageInTableBox() {
        return el(By.xpath(PAGE_IN_TABLE_BOX_XPATH));
    }

    /**
     * Gets the first page link in table
     *
     * @return the link element
     */
    private El getFirstPageLink() {
        return el(By.xpath(FIRST_PAGE_IN_TABLE_LINK_XPATH));
    }

    /**
     * Gets the previous page link in table
     *
     * @return the link element
     */
    private El getPreviousPageLink() {
        return el(By.xpath(PREVIOUS_PAGE_IN_TABLE_LINK_XPATH));
    }

    /**
     * Gets the next page link in table
     *
     * @return the link element
     */
    private El getNextPageLink() {
        return el(By.xpath(NEXT_PAGE_IN_TABLE_LINK_XPATH));
    }

    /**
     * Gets the last page link in table
     *
     * @return the link element
     */
    private El getLastPageLink() {
        return el(By.xpath(LAST_PAGE_IN_TABLE_LINK_XPATH));
    }

    /**
     * Gets the entries per page drop down selector
     *
     * @return the entries per page drop down selector
     */
    private El getEntriesPerPageDropdownSelector() {
        return el(By.xpath(ENTRIES_PER_PAGE_DROPDOWN_SELECTOR_XPATH));
    }

    /**
     * Gets the specified option from the Entries per page drop down
     *
     * @param entriesOption -the option to get
     * @return the option element
     */
    private El getEntriesPerPageOption(EntriesPerPage entriesOption) {
        return el(By.xpath(DROPDOWN_SPECIFIC_OPTION_XPATH_PREFIX + entriesOption.label + "']"));
    }

    /**
     * Gets the option selected in the Entries per page drop down
     *
     * @return the option element
     */
    private El getSelectedEntriesPerPage() {
        return el(By.xpath(SELECTED_OPTION_XPATH));
    }

    /**
     * Gets an element to verify there are no search results to display when
     * searching
     *
     * @return the Element associated with the xpath
     */
    private El getNoDataXPath() {
        return el(By.xpath(NO_SEARCH_RESULTS_XPATH));
    }

    /**
     * Finds the row in table with the given text
     *
     * @param columnCell - By of column cell where text is expected
     * @param text - text expected in the table
     * @return the row in which the given text is displayed
     */
    private El rowInSearchResultsTableWithColumnText(By columnCellBy, String text) {
        waitUntilLoaded();
        List<El> rows = getSearchResultRows();
        El theRow = null;
        Iterator<El> iterator = rows.iterator();
        while (iterator.hasNext() && theRow == null) {
            El row = iterator.next();
            waitUntilLoaded();
            List<El> cells = row.els(columnCellBy);
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
     * Gets row in the Search results table (including all subsequent pages)
     * containing the given text in Key column
     *
     * @param text -the text to find
     * @return the row containing the given text
     */
    private El getRowInSearchResultsTableWithKeyText(String text) {
        El theRow = null;
        El nextBtn = getNextPageLink();
        theRow = rowInSearchResultsTableWithColumnText(By.xpath(KEY_COLUMN_IN_ROW_XPATH), text);
        if (theRow == null && isPaginationButtonEnabled(By.xpath(NEXT_PAGE_IN_TABLE_LINK_XPATH))) {
            nextBtn.click();
            waitUntilLoaded();
            nextBtn = getNextPageLink(); //refresh the Next link
            theRow = getRowInSearchResultsTableWithKeyText(text);
        }
        return theRow;
    }

    /**
     * Gets the search criteria list with the given name
     *
     * @param listName -the name of the list to get
     * @return the list element
     */
    private El getSearchCriteriaList(SearchCriteriaListName listName) {
        return el(By.xpath(LIST_XPATH_PREFIX + listName.label + LIST_XPATH_SUFFIX));
    }

    /**
     * Gets the given option from the given search criteria list
     *
     * @param listName -the name of the list
     * @param option -the option to get in the list
     * @return the row in the list containing the given option
     */
    private El getOptionInSearchCriteriaList(SearchCriteriaListName listName, String option) {
        El list = getSearchCriteriaList(listName);
        return list.el(By.xpath(ROW_IN_LIST_XPATH_PREFIX + option + "']"));
    }

    /**
     * Gets selected options in Search Criteria lists
     *
     * @return the list of options currently selected
     */
    private List<El> getSelectedSearchCriteriaOptions() {
        return els(By.xpath(SELECTED_OPTION_IN_LIST_XPATH));
    }

    /**
     * Gets the displayed value in the current page text box
     *
     * @return the displayed page number
     */
    public int currentPage() {
        String page = getPageInTableBox().getAttribute("value");
        return Integer.parseInt(page);
    }

    /**
     * Gets the displayed value in the total pages
     *
     * @return the displayed page number
     */
    public int totalPages() {
        String total = (getTotalPagesBox().getText()).substring(3);
        return Integer.parseInt(total);
    }

    /**
     * Gets the value selected in the entries per page drop down; first checks
     * if a selected option can be found - if not drop down is closed and null
     * is returned. NOTE: This method also clicking the entries per page drop
     * down to expand and then clicks to close
     *
     * @return the displayed entries per page value
     */
    public String getDisplayedEntriesPerPageOption() {
        clickEntriesPerPageDropdown();
        El selectedOption = getSelectedEntriesPerPage();
        if (selectedOption.exists()) {
            String optionText = selectedOption.getText();
            clickEntriesPerPageDropdown();
            return optionText;
        }
        else {
            clickEntriesPerPageDropdown();
            return null;
        }
    }

    //////////////
    // Checkers
    //////////////
    /**
     * Checks if icon to navigate to first page is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isFirstPageIconDisplayed() {
        return isElementDisplayed(getFirstPageLink());
    }

    /**
     * Checks if icon to navigate to previous page is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isPreviousPageIconDisplayed() {
        return isElementDisplayed(getPreviousPageLink());
    }

    /**
     * Checks if icon to navigate to next page is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isNextPageIconDisplayed() {
        return isElementDisplayed(getNextPageLink());
    }

    /**
     * Checks if icon to navigate to last page is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isLastPageIconDisplayed() {
        return isElementDisplayed(getLastPageLink());
    }

    /**
     * Checks if Entries Per Page drop down is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isEntriesPerPageDropDownDisplayed() {
        return isElementDisplayed(getEntriesPerPageDropdownSelector());
    }

    /**
     * Checks if icon to navigate to first page is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isFirstPageIconDisabled() {
        String displayedClass = getFirstPageLink().getAttribute("class");
        return displayedClass.contains("x-item-disabled");
    }

    /**
     * Checks if icon to navigate to previous page is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isPreviousPageIconDisabled() {
        String displayedClass = getPreviousPageLink().getAttribute("class");
        return displayedClass.contains("x-item-disabled");
    }

    /**
     * Checks if icon to navigate to next page is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isNextPageIconDisabled() {
        String displayedClass = getNextPageLink().getAttribute("class");
        return displayedClass.contains("x-item-disabled");
    }

    /**
     * Checks if icon to navigate to last page is disabled
     *
     * @return true if disabled, false if enabled
     */
    public boolean isLastPageIconDisabled() {
        String displayedClass = getLastPageLink().getAttribute("class");
        return displayedClass.contains("x-item-disabled");
    }

    /**
     * Checks if given app is displayed in results
     *
     * @param appName -the name of the app expected
     * @return true if displayed, false otherwise
     */
    public boolean isAppDisplayedInResults(String appName) {
        El theRow = null;
        List<El> rows = getSearchResultRows();
        for (El row : rows) {
            String displayedApp = getDisplayedApp(row);
            if (displayedApp.equals(appName)) {
                theRow = row;
                break;
            }
        }
        return theRow != null;
    }

    /**
     * Checks if Search Criteria list is displayed
     *
     * @param listName -the name of the search criteria list to check
     * @return true if displayed, false otherwise
     */
    public boolean isSearchCriteriaListDisplayed(SearchCriteriaListName listName) {
        return isElementDisplayed(getSearchCriteriaList(listName));
    }

    /**
     * Checks if given option is displayed in given search criteria list
     *
     * @param listName -the search criteria list to check
     * @param option -the option expected in the list
     * @return true if the given option is displayed in the given search
     *         criteria list, false otherwise
     */
    public boolean isSearchCriteriaOptionDisplayed(SearchCriteriaListName listName, String option) {
        El displayedOption = getOptionInSearchCriteriaList(listName, option);
        return displayedOption != null;
    }

    /**
     * Checks if any search criteria options are selected
     *
     * @return true if selections are found, false otherwise
     */
    public boolean areSearchCriteriaOptionsSelected() {
        String size = Integer.toString(getSelectedSearchCriteriaOptions().size());
        return size != null;
    }

    /**
     * Checks xpath to verify no data is displayed in search results
     *
     * @return true if 'No data to display' is visible after searching, false if
     *         not
     */
    public boolean checkNoSearchData() {
        return isElementDisplayed(getNoDataXPath());
    }

    /**
     * Checks to see if Invalid Min Sim Time error is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInvalidMinSimTimeErrorDisplayed() {
        return isInvalidNumberErrorDisplayed(MIN_SIM_TIME_FIELD_NAME);
    }

    ////////////////
    // Interaction
    ///////////////
    /**
     * Clicks the first page link in a table and waits for page to load
     */
    public void clickFirstPage() {
        getFirstPageLink().click();
        driverWait(PAGE_LOAD_TIMEOUT).until(ExpectedConditions.attributeToBe(By.xpath(PAGE_IN_TABLE_BOX_XPATH), "value", "1"));
    }

    /**
     * Clicks the previous page link in a table and waits for page to load
     */
    public void clickPreviousPage() {
        int currentPage = currentPage();
        getPreviousPageLink().click();
        driverWait(PAGE_LOAD_TIMEOUT).until(ExpectedConditions.attributeToBe(By.xpath(PAGE_IN_TABLE_BOX_XPATH), "value", Integer.toString(currentPage - 1)));
    }

    /**
     * Clicks the next page link in a table and waits for page to load
     */
    public void clickNextPage() {
        int currentPage = currentPage();
        getNextPageLink().click();
        driverWait(PAGE_LOAD_TIMEOUT).until(ExpectedConditions.attributeToBe(By.xpath(PAGE_IN_TABLE_BOX_XPATH), "value", Integer.toString(currentPage + 1)));
    }

    /**
     * Clicks the last page link in a table and waits for page to load
     */
    public void clickLastPage() {
        int totalPages = totalPages();
        getLastPageLink().click();
        driverWait(PAGE_LOAD_TIMEOUT).until(ExpectedConditions.attributeToBe(By.xpath(PAGE_IN_TABLE_BOX_XPATH), "value", Integer.toString(totalPages)));
    }

    /**
     * Clicks the entries per page dropdown
     */
    public void clickEntriesPerPageDropdown() {
        getEntriesPerPageDropdownSelector().click();
    }

    /**
     * Checks that the expected option is displayed in the given list, then
     * click the option
     *
     * @param listName - the name of the search criteria list
     * @param option -the name of the option
     */
    public void selectOptionInSearchCriteriaList(SearchCriteriaListName listName, String option) {
        Assert.assertTrue("Expected option, '" + option + "', could not be found in the list, " + listName.label + ".", isSearchCriteriaOptionDisplayed(listName, option));
        El listOption = getOptionInSearchCriteriaList(listName, option);
        listOption.click();
    }

    /////////////
    // Utilities
    /////////////
    /**
     * Gets the number of rows displayed in the Search results table on the
     * current page
     *
     * @return the number of rows displayed
     */
    public int getCountOfAppLogSearchRowsDisplayed() {
        return getSearchResultRows().size();
    }

    /**
     * Selects the given number from the entries per page dropdown (method
     * handles clicking dropdown to expand and then clicking the given option)
     *
     * @param value -the entries per page value to select
     */
    public void selectEntriesPerPage(EntriesPerPage value) {
        clickEntriesPerPageDropdown();
        getEntriesPerPageOption(value).click();
        clickSearchBtn();
    }

    /**
     * Checks initial state of pagination controls in table (Checks that page 1
     * is displayed, 25 is selected from Entries Per Page dropdown, First and
     * previous page icons are disabled - also checks if total pages is greater
     * than 1 and if so checks that next and last icons are enabled otherwise
     * ensures they are disabled)
     */
    public void checkPagination() {
        Assert.assertEquals("The current page is not displayed as '1' as expected when first page of table is displayed.", 1, currentPage());
        Assert.assertEquals("The items per page is not displayed as expected when first page of table is  displayed.", "25", getDisplayedEntriesPerPageOption());
        Assert.assertTrue("First page icon not disabled as expected.", isFirstPageIconDisabled());
        Assert.assertTrue("Previous page icon not disabled as expected.", isPreviousPageIconDisabled());
        int totalPages = totalPages();
        if (totalPages > 1) {
            Assert.assertFalse("Next page icon not disabled as expected.", isNextPageIconDisabled());
            Assert.assertFalse("Last page icon not disabled as expected.", isLastPageIconDisabled());
        }
        else {
            Assert.assertTrue("Next page icon not disabled as expected.", isNextPageIconDisabled());
            Assert.assertTrue("Last page icon not disabled as expected.", isLastPageIconDisabled());
        }
    }

    /**
     * Checks for presence of all pagination controls (First page icon, Previous
     * Page icon, Next page icon, Last Page icon, and Entries per page drop
     * down)
     */
    public void checkPaginationControlsDisplayed() {
        Assert.assertTrue("First page icon not displayed as expected.", isFirstPageIconDisplayed());
        Assert.assertTrue("Previous page icon not displayed as expected.", isPreviousPageIconDisplayed());
        Assert.assertTrue("Next page icon not displayed as expected.", isNextPageIconDisplayed());
        Assert.assertTrue("Last page icon not displayed as expected.", isLastPageIconDisplayed());
        Assert.assertTrue("Entries Per Page drop down not displayed as expected.", isEntriesPerPageDropDownDisplayed());
    }

    /**
     * Checks if more than one page is available in table and navigates to last
     * page of table (if more than one page)
     */
    public void goToLastPageInTable() {
        waitUntilLoaded();
        if (currentPage() != totalPages()) {
            clickLastPage();
        }
        Assert.assertEquals("Current page does not display as expected when Last is clicked.", totalPages(), currentPage());
    }

    /**
     * Checks for functionality of pagination controls (clicking next, previous,
     * last, and first, and changing items displayed per page)
     */
    public void checkPaginationFunctionality() {
        clickNextPage();
        Assert.assertEquals("Current page does not display as expected when Next is clicked.", 2, currentPage());
        clickPreviousPage();
        Assert.assertEquals("Current page does not display as expected when Previous is clicked.", 1, currentPage());
        clickLastPage();
        Assert.assertEquals("Current page does not display as expected when Last is clicked.", totalPages(), currentPage());
        clickFirstPage();
        Assert.assertEquals("Current page does not display as expected when First is clicked.", 1, currentPage());
        int currentNumOfPages = totalPages();
        selectEntriesPerPage(EntriesPerPage.FIFTY);
        Assert.assertTrue("Total number of pages does not display as expected when items per page is altered.", totalPages() < currentNumOfPages);
    }

    /**
     * Checks for given value in first row
     *
     * @param cellValue -the value expected in the first row
     */
    public void checkFirstRow(String cellValue) {
        Assert.assertTrue("Cell with value of, " + cellValue + ", was not displayed in first row.", isElementDisplayed(getCellInRow(getFirstSearchResultRow(), cellValue)));
    }

    /**
     * Checks for given value in last row on given page
     *
     * @param cellValue -the value expected to be displayed in the last row
     */
    public void checkLastRow(String cellValue) {
        List<El> rows = getSearchResultRows();
        int rowsDisplayed = rows.size();
        El row = rows.get(rowsDisplayed - 1);
        Assert.assertTrue("Cell with value of, " + cellValue + ", not displayed.", isElementDisplayed(getCellInRow(row, cellValue)));
    }

    /**
     * Checks that all displayed rows have values populated in the Device column
     */
    public void checkAllRowsDeviceColumnPopulated() {
        List<El> rows = getSearchResultRows();
        for (El row : rows) {
            Assert.assertTrue("Device column not populated as expected.", getDisplayedDevice(row).length() > 0);
        }
    }

    /**
     * Checks that all displayed rows have the given value populated in the App
     * column
     *
     * @param appName -the name of the App expected
     */
    public void checkAllRowsAppColumn(String appName) {
        List<El> rows = getSearchResultRows();
        for (El row : rows) {
            Assert.assertEquals("App column not populated with the name, " + appName + ", as expected.", getDisplayedApp(row), appName);
        }
    }

    /**
     * Checks that all displayed rows have values populated in the Sim Time
     * column
     */
    public void checkAllRowsSimTimeColumnPopulated() {
        List<El> rows = getSearchResultRows();
        for (El row : rows) {
            Assert.assertTrue("Sim Time column not populated as expected.", getDisplayedSimTime(row).length() > 0);
        }
    }

    /**
     * Checks that rows are present with Vehicle data in the Key column (in the
     * expected formats of Vehicle: :X; Vehicle: :Y, and Vehicle: :Z)
     */
    public void checkVehicleKeyRows() {
        El rows = null;
        while (rows == null && isNextPageIconDisabled() == false) {
            getNextPageLink().click();
        }
    }

    /**
     * Verifies row is displayed with expected value in Key column
     *
     * @param expectedKey - value expected in Key column
     */
    public void checkKeyValueInRowDisplayed(String expectedKey) {
        El row = getRowInSearchResultsTableWithKeyText(expectedKey);
        Assert.assertNotNull("Row with Key of " + expectedKey + " could not be found.", row);
        Assert.assertTrue(expectedKey + "could not be found in Key column.", isElementDisplayed(row));
    }

    /**
     * Verifies row is displayed with expected value in Data column
     *
     * @param expectedKey - value expected in Key column (this value used to
     *        find the row)
     * @param expectedData - value expected in Data column
     */
    public void checkKeyAndDataValueInRowDisplayed(String expectedKey, String expectedData) {
        El row = getRowInSearchResultsTableWithKeyText(expectedKey);
        Assert.assertNotNull("Row with Key of " + expectedKey + " could not be found.", row);
        El dataCell = row.el(By.xpath(DATA_COLUMN_IN_ROW_XPATH));
        Assert.assertNotNull("Data cell could not be found in row with key, " + expectedKey + ".", dataCell);
        String data = dataCell.getText();
        Assert.assertEquals(expectedData + "could not be found in Data column.", expectedData, data);
    }

    //////////
    // Waits
    /////////
    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(APP_LOG_AREA_XPATH));
    }
}
