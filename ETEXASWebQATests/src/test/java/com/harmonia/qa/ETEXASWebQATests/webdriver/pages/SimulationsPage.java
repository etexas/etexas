package com.harmonia.qa.ETEXASWebQATests.webdriver.pages;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Simulation.SimulationFoundation;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UploadedSimulation;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.ConfirmDeleteModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.CompositeSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DeviceApplicationsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulationSettings.SimulationSettingsModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CreateSimulationFromTemplateModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.simulations.CreateSimulationFromUploadModal;
import com.harmonia.qa.webdriver.utilities.elements.El;

import junit.framework.Assert;

/**
 * Page class representing the eTexas Simulations Page
 *
 * @author llaroussini
 */
public class SimulationsPage extends DashboardPage {

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public SimulationsPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Buttons associated with Simulation Configuration on the Simulations page
     *
     * @author llaroussini
     * @author rsmith
     */
    public enum SimBtns {
        /**
         * The Create button associated with a Simulation
         */
        CREATE_BTN("Create", "create"),
        /**
         * The Edit button associated with a Simulation
         */
        EDIT_BTN("Edit", "edit"),
        /**
         * The Delete button associated with a Simulation
         */
        DELETE_BTN("Delete", "delete");

        /**
         * The label of the buttons as they appear in the application
         */
        private String label;

        /**
         * The unique ID associated with the buttons
         */
        private String id;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         * @param id The unique ID associated with the button
         */
        SimBtns(String label, String id) {
            this.label = label;
            this.id = id;
        }

        /**
         * Gets the label associated with the button as it is displayed in the
         * Web UI
         *
         * @return The label of the button
         */
        public String getLabel() {
            return this.label;
        }

        /**
         * Gets the unique ID associated with the button
         *
         * @return The ID associated with the button
         */
        public String getID() {
            return this.id;
        }
    }

    /**
     * Option links listed under Create button on the Simulations page
     *
     * @author llaroussini
     */
    public enum CreateSimOptions {
        /**
         * The Template new simulation option
         */
        TEMPLATE_OPTION("from Template"),
        /**
         * The Upload new simulation option
         */
        UPLOAD_OPTION("from Upload");

        /**
         * The label of the option links as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        CreateSimOptions(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the option link as it is displayed in
         * the Web UI
         *
         * @return The label of the option link
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * Option links listed under Edit button on the Simulations page
     *
     * @author llaroussini
     */
    public enum EditOptions {
        /**
         * The Copy option
         */
        COPY("Copy"),
        /**
         * The Export option
         */
        EXPORT("Export"),
        /**
         * The Rename option
         */
        RENAME("Rename"),
        /**
         * The Composite Settings option
         */
        COMPOSITE_SETTINGS("Composite Settings"),
        /**
         * The Simulation Settings option
         */
        SIMULATION_SETTINGS("Simulation Settings"),
        /**
         * The Reporting option
         */
        REPORTING("Reporting"),
        /**
         * The Simulation Source option
         */
        SIMULATION_SOURCE("Simulation Source");

        /**
         * The label of the option links as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        EditOptions(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the link as it is displayed in the Web
         * UI
         *
         * @return The label of the link
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * Enum for names of columns on page
     *
     * @author llaroussini
     */
    public enum ColumnName {
        /**
         * ID column
         */
        ID("ID"),
        /**
         * Name column
         */
        NAME("Name"),
        /**
         * Type column
         */
        TYPE("Type"),
        /**
         * Source column
         */
        SOURCE("Source");

        /**
         * The label of the column name as it appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        ColumnName(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the column name as it is displayed in
         * the Web UI
         *
         * @return The label of the column name
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * Enum for sorting option in sorting menu
     *
     * @author llaroussini
     */
    public enum SortingOption {
        /**
         * Sort Ascending option
         */
        SORT_ASCENDING("Sort Ascending"),
        /**
         * Sort Descending option
         */
        SORT_DESCENDING("Sort Descending"),
        /**
         * Columns option
         */
        COLUMNS("Columns"),
        /**
         * ID option - in Columns sub-menu
         */
        ID("ID"),
        /**
         * Name option - in Columns sub-menu
         */
        NAME("Name"),
        /**
         * Type option - in Columns sub-menu
         */
        TYPE("Type"),
        /**
         * Source option - in Columns sub-menu
         */
        SOURCE("Source");

        /**
         * The label of the menu option as it appears in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        SortingOption(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the sorting menu option as it is
         * displayed in the Web UI
         *
         * @return The label of the option name
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath for Simulations header
     */
    private static final String SIMULATIONS_HEADER_XPATH = "//div[contains(@id, 'navigation-toolbar')]/a[contains(@id, 'simulations-button')]//span[text()='Simulations']";

    /**
     * Header text displayed on Simulations page
     */
    private static final String SIMULATIONS_HEADER_TEXT = "Simulations";

    /**
     * Xpath prefix assigned to simulation/execution buttons
     */
    private static final String BUTTON_XPATH_PREFIX = "//span[contains(@id, 'simulation')][contains(@id, '";

    /**
     * Xpath prefix to the title cell for a column
     */
    private static final String COLUMN_TITLE_CELL_XPATH_PREFIX = "//div[contains(@class, 'x-title-text')][text()='Simulation Configurations']/ancestor::div[contains(@class, 'x-grid')]//span[@class='x-column-header-text'][text()='";

    /**
     * Xpath prefix to a specific cell
     */
    private static final String CELL_XPATH_PREFIX = "//div[contains(@id, 'ETexas-application-view-Perspective-simulation-tree-panel')]//td[contains(@class, 'x-grid-cell-ETexas-simulation-view-List-name-column')]//span[text()='";

    /**
     * Xpath to ID cell, used in conjunction with row element (finds ID cell in
     * a given row when used together)
     */
    private static final String ID_CELL_XPATH = ".//td[contains(@class, 'x-grid-cell-first')]";

    /**
     * Xpath to Type cell, used in conjunction with row element (finds ID cell
     * in a given row when used together)
     */
    private static final String TYPE_CELL_XPATH = ".//td[3]";

    /**
     * Xpath to Source cell, used in conjunction with row element (finds ID cell
     * in a given row when used together)
     */
    private static final String SOURCE_CELL_XPATH = ".//td[4]";

    /**
     * Xpath prefix assigned to menu option
     */
    private static final String MENU_OPTION_XPATH_PREFIX = "//span[contains(@id, 'menu')][text()='";

    /**
     * Xpath suffix assigned to menu option
     */
    private static final String MENU_OPTION_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath suffix for parent row of a cell in table
     */
    private static final String PARENT_ROW_XPATH_SUFFIX = "/ancestor::tr";

    /**
     * Xpath to the parent div of button designating status (enabled/disabled)
     */
    private static final String PARENT_BUTTON_STATUS_DIV_SUFFIX = "/ancestor::a";

    /**
     * Xpath of the Sorting Arrow icon displayed in column header cells
     */
    private static final String SORTING_ARROW_ICON_XPATH = ".//div[@class='x-column-header-trigger']";

    /**
     * Xpath to the sorting menu
     */
    private static final String SORTING_MENU_XPATH = "//div[contains(@class, 'x-box-scroller-body-vertical')][not(contains(@style, 'visibility: hidden;'))]";

    /**
     * Xpath prefix to sorting menu option
     */
    private static final String SORTING_MENU_OPTION_XPATH_PREFIX = "//div[contains(@class, 'x-box-scroller-body-vertical')][not(contains(@style, 'visibility: hidden;'))]//span[contains(@class, 'x-menu-item-indent-no-separator')][text()='";

    /**
     * The xpath suffix for sorting menu options
     */
    private static final String SORTING_MENU_OPTION_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath to use when getting check box (use with sorting menu prefix)
     */
    private static final String SORTING_MENU_OPTION_CHECKBOX_XPATH_SUFFIX = "']/ancestor::a/div";

    /**
     * Xpath to the name filter text box
     */
    private static final String NAME_FILTER_TEXT_BOX_XPATH = "//input[@placeholder='Enter Filter Text...']";

    /**
     * Xpath to determine the selected state of a check box
     */
    private static final String SELECTED_STATE_CHECKBOX_XPATH = "./ancestor::node()[2]";

    /**
     * Class displayed when a simulation or composite is selected
     */
    private static final String SELECTED_COMPSITE_SIM_CLASS = "x-grid-item-selected";

    /**
     * Xpath to expand icon associated with composite
     */
    private static final String COMPOSITE_EXPAND_ICON_XPATH = ".//div[contains(@class, 'x-tree-expander')]";

    /**
     * String constant for the 'class' attribute, used throughout page class
     */
    private static final String ATTR_CLASS = "class";

    /**
     * Constant for the attribute 'aria-expanded'
     */
    private static final String ARIA_EXPANDED = "aria-expanded";

    /**
     * String constant for the attribute 'aria-disabled'
     */
    private static final String ATTR_ARIA_DISABLED = "aria-disabled";

    /**
     * String constant for the attribute 'aria-selected'
     */
    private static final String ATTR_ARIA_SELECTED = "aria-selected";

    /**
     * Xpath prefix assigned to simulation button
     */
    private static final String SIM_BUTTON_XPATH_PREFIX = "//span[contains(@id, 'simulation')][@data-ref='btnInnerEl'][text()='";

    /**
     * String constant for the 'disabled' attribute, used throughout page class
     */
    private static final String ATTR_DISABLED = "disabled";

    ///////////
    //Element Getters
    ///////////

    /**
     * Gets the header for the simulations section
     *
     * @return the simulation configurations header
     */
    private El getSimulationsHeader() {
        return el(By.xpath(SIMULATIONS_HEADER_XPATH));
    }

    /**
     * Gets given button in simulations panel
     *
     * @param btn -the button to get
     * @return the button element
     */
    private El getBtn(SimBtns btn) {
        String id = btn.getID();
        return el(By.xpath(BUTTON_XPATH_PREFIX + id + "')]"));
    }

    /**
     * Gets given option in edit button menu
     *
     * @param option -the option to get
     * @return the option link element
     */
    private El getEditOption(EditOptions option) {
        String optionName = option.getLabel();
        return el(By.xpath(MENU_OPTION_XPATH_PREFIX + optionName + MENU_OPTION_XPATH_SUFFIX));
    }

    /**
     * Gets given option in Create button menu (i.e., Template or Upload in New
     * button menu)
     *
     * @param option -the option to get
     * @return the option link element
     */
    private El getOption(CreateSimOptions option) {
        String optionName = option.getLabel();
        return el(By.xpath(MENU_OPTION_XPATH_PREFIX + optionName + "']"));
    }

    /**
     * Gets the template link (part of new simulation options)
     *
     * @return the template link
     */
    private El getTemplateLink() {
        return getOption(CreateSimOptions.TEMPLATE_OPTION);
    }

    /**
     * Gets the upload option (part of new simulation options)
     *
     * @return the upload option
     */
    private El getUploadOption() {
        return getOption(CreateSimOptions.UPLOAD_OPTION);
    }

    /**
     * Gets the title cell for a column
     *
     * @param name -the title of the cell
     * @return the cell element
     */
    private El getColumnTitleCell(ColumnName name) {
        String displayedName = name.getLabel();
        return el(By.xpath(COLUMN_TITLE_CELL_XPATH_PREFIX + displayedName + "']"));
    }

    /**
     * Gets a simulation or composite row
     *
     * @param name -the name of the simulation or composite
     * @return the row element
     */
    private El getSimCompositeRow(String name) {
        String cellXpath = CELL_XPATH_PREFIX + name + "']";
        return el(By.xpath(cellXpath + PARENT_ROW_XPATH_SUFFIX));
    }

    /**
     * Gets the row from a cell with the given simulation name
     *
     * @param sim -the simulation expected
     * @return the row element
     */
    private El getSimRow(TemplateSimulation sim) {
        String simName = sim.getName();
        return getSimCompositeRow(simName);
    }

    /**
     * Gets the row from a cell with the given simulation name
     *
     * @param sim -the simulation expected
     * @return the row element
     */
    private El getSimRow(UploadedSimulation sim) {
        String simName = sim.getName();
        return getSimCompositeRow(simName);
    }

    /**
     * Gets the sorting arrow icon displayed in given simulation column header
     * cell
     *
     * @param name -the title of the column where the sorting arrow icon is
     *        expected
     * @return the icon element
     */
    private El getColumnHeaderSortingArrowIcon(ColumnName name) {
        return getColumnTitleCell(name).el(By.xpath(SORTING_ARROW_ICON_XPATH));
    }

    /**
     * Gets the row from a cell with the composite name
     *
     * @param composite -the composite expected
     * @return the row element
     */
    private El getCompositeRow(CompositeSimulation composite) {
        String compositeName = composite.getName();
        return getSimCompositeRow(compositeName);
    }

    /**
     * Gets cell in table with given composite name
     *
     * @param compositeName -the composite name expected
     * @return the cell element
     */
    private El getCompositeNameCell(String compositeName) {
        return el(By.xpath(CELL_XPATH_PREFIX + compositeName + "']"));
    }

    /**
     * Gets cell in table with given simulation name
     *
     * @param simName -the simulation name expected
     * @return the cell element
     */
    private El getSimNameCell(String simName) {
        return el(By.xpath(CELL_XPATH_PREFIX + simName + "']"));
    }

    /**
     * Gets the sorting menu
     *
     * @return the menu element
     */
    private El getSortingMenu() {
        return el(By.xpath(SORTING_MENU_XPATH));
    }

    /**
     * Gets the given sorting option in the sorting menu
     *
     * @param option -the option to find
     * @return the sorting option element
     */
    private El getSortingOption(SortingOption option) {
        String optionName = option.getLabel();
        return el(By.xpath(SORTING_MENU_OPTION_XPATH_PREFIX + optionName + SORTING_MENU_OPTION_XPATH_SUFFIX));
    }

    /**
     * Gets the given sorting option's check box
     *
     * @param option -the option expected to have check box
     * @return the check box element
     */
    private El getSortingItemCheckBox(SortingOption option) {
        String optionName = option.getLabel();
        return el(By.xpath(SORTING_MENU_OPTION_XPATH_PREFIX + optionName + SORTING_MENU_OPTION_CHECKBOX_XPATH_SUFFIX));
    }

    /**
     * Gets the text box displayed in Name sorting filter
     *
     * @return the text box element
     */
    private El getSortingNameTextBox() {
        return el(By.xpath(NAME_FILTER_TEXT_BOX_XPATH));
    }

    /**
     * Gets displayed ID for given composite
     *
     * @param composite -the composite
     * @return the ID as string
     */
    public String getCompositeID(CompositeSimulation composite) {
        El idCell = getCompositeRow(composite).el(By.xpath(ID_CELL_XPATH));
        return idCell.getText();
    }

    /**
     * Gets displayed ID for given simulation
     *
     * @param sim -the simulation
     * @return the ID as string
     */
    public String getSimID(TemplateSimulation sim) {
        El idCell = getSimRow(sim).el(By.xpath(ID_CELL_XPATH));
        return idCell.getText();
    }

    /**
     * Gets displayed ID for given simulation
     *
     * @param sim -the simulation
     * @return the ID as string
     */
    public String getSimID(UploadedSimulation sim) {
        El idCell = getSimRow(sim).el(By.xpath(ID_CELL_XPATH));
        return idCell.getText();
    }

    /**
     * Gets displayed Type for given simulation
     *
     * @param sim -the simulation
     * @return the ID as string
     */
    public String getSimType(TemplateSimulation sim) {
        El typeCell = getSimRow(sim).el(By.xpath(TYPE_CELL_XPATH));
        return typeCell.getText();
    }

    /**
     * Gets displayed Type for given simulation
     *
     * @param sim -the simulation
     * @return the ID as string
     */
    public String getSimType(UploadedSimulation sim) {
        El typeCell = getSimRow(sim).el(By.xpath(TYPE_CELL_XPATH));
        return typeCell.getText();
    }

    /**
     * Gets displayed Source for given simulation
     *
     * @param sim -the simulation
     * @return the ID as string
     */
    public String getSimSource(TemplateSimulation sim) {
        El sourceCell = getSimRow(sim).el(By.xpath(SOURCE_CELL_XPATH));
        return sourceCell.getText();
    }

    /**
     * Gets displayed Source for given simulation
     *
     * @param sim -the simulation
     * @return the ID as string
     */
    public String getSimSource(UploadedSimulation sim) {
        El sourceCell = getSimRow(sim).el(By.xpath(SOURCE_CELL_XPATH));
        return sourceCell.getText();
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks to see if the simulation configurations header is displayed
     *
     * @return true if the simulation configuration header is displayed, false
     *         otherwise
     */
    public boolean isSimulationsHeaderDisplayed() {
        return isElementDisplayed(getSimulationsHeader());
    }

    /**
     * Checks to see if given simulation button is displayed
     *
     * @param btn the button which is being checked
     * @return true if given button is displayed, false otherwise
     */
    public boolean isBtnDisplayed(SimBtns btn) {
        return isElementDisplayed(getBtn(btn));
    }

    /**
     * Checks to see if the given button is enabled
     *
     * @param btn the button which is being checked
     * @return true if the given button is enabled, false otherwise
     */
    public boolean isBtnEnabled(SimBtns btn) {
        String btnID = btn.getID();
        El simButton = el(By.xpath(BUTTON_XPATH_PREFIX + btnID + "')]" + PARENT_BUTTON_STATUS_DIV_SUFFIX));
        String elClass = simButton.getAttribute(ATTR_CLASS);
        return !(elClass.contains("disabled"));
    }

    /**
     * Checks to see if the template option is displayed
     *
     * @return true if the template option is displayed, false otherwise
     */
    public boolean isTemplateOptionDisplayed() {
        return isElementDisplayed(getTemplateLink());
    }

    /**
     * Checks to see if the upload option is displayed
     *
     * @return true if the upload option is displayed, false otherwise
     */
    public boolean isUploadOptionDisplayed() {
        return isElementDisplayed(getUploadOption());
    }

    /**
     * Checks to see if the given simulation is displayed in the table
     *
     * @param sim -the simulation expected
     * @return true if displayed, false otherwise
     */
    public boolean isSimDisplayed(TemplateSimulation sim) {
        String simName = sim.getName();
        return isSimDisplayed(simName);
    }

    /**
     * Checks to see if the given simulation is displayed in the table
     *
     * @param sim -the simulation expected
     * @return true if displayed, false otherwise
     */
    public boolean isSimDisplayed(UploadedSimulation sim) {
        String simName = sim.getName();
        return isSimDisplayed(simName);
    }

    /**
     * Checks to see if the given simulation name is displayed in the table
     *
     * @param simName -name of the simulation expected
     * @return true if displayed, false otherwise
     */
    public boolean isSimDisplayed(String simName) {
        return isElementDisplayed(getSimNameCell(simName));
    }

    /**
     * Checks to see if the given edit option is displayed
     *
     * @param option -the option expected
     * @return true if the edit option is displayed, false otherwise
     */
    public boolean isEditOptionDisplayed(EditOptions option) {
        return isElementDisplayed(getEditOption(option));
    }

    /**
     * Checks to see if the given edit option is enabled
     *
     * @param option -the option expected
     * @return true if the edit option is enabled, false otherwise
     */
    public boolean isEditOptionEnabled(EditOptions option) {
        El editOption = getEditOption(option);
        String aria = editOption.getAttribute(ATTR_ARIA_DISABLED);
        return aria.equals("false");
    }

    /**
     * Checks if given simulation type is displayed in the same row as the given
     * simulation name
     *
     * @param simName -template simulation name expected
     * @param simType -template simulation type expected
     * @return true if type is displayed in the same row as the name; false if
     *         not
     */
    public boolean isSimTypeDisplayed(String simName, String simType) {
        El row = getSimCompositeRow(simName);
        try {
            El typeCell = row.el(getCell(simType));
            return typeCell.isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if given simulation type is displayed in the same row as the given
     * simulation
     *
     * @param sim -template simulation expected
     * @return true if type is displayed in the same row as the name; false if
     *         not
     */
    public boolean isSimTypeDisplayed(TemplateSimulation sim) {
        String simName = sim.getName();
        String simType = sim.getSimType().getLabel();
        return isSimTypeDisplayed(simName, simType);
    }

    /**
     * Checks to see if the name of the given composite is displayed in the
     * table
     *
     * @param composite -composite expected
     * @return true if displayed, false if not
     */
    public boolean isCompositeDisplayed(CompositeSimulation composite) {
        String compositeName = composite.getName();
        return isCompositeNameDisplayed(compositeName);
    }

    /**
     * Checks if given composite is selected
     *
     * @param composite -the composite to check
     * @return true if selected, false otherwise
     */
    public boolean isCompositeSelected(CompositeSimulation composite) {
        El compositeRow = getCompositeRow(composite);
        String compositeClass = compositeRow.el(By.xpath("./ancestor::table")).getAttribute("class");
        return compositeClass.contains(SELECTED_COMPSITE_SIM_CLASS);
    }

    /**
     * Checks if given composite is expanded
     *
     * @param composite -the composite to check
     * @return true if expanded, false otherwise
     */
    public boolean isCompositeExpanded(CompositeSimulation composite) {
        El compositeRow = getCompositeRow(composite);
        String ariaExpanded = compositeRow.getAttribute(ARIA_EXPANDED);
        return "true".equals(ariaExpanded);
    }

    /**
     * Checks if given simulation is selected
     *
     * @param sim -the simulation to check
     * @return true if selected, false otherwise
     */
    public boolean isSimulationSelected(TemplateSimulation sim) {
        El simRow = getSimRow(sim);
        String simClass = simRow.getAttribute("class");
        return simClass.contains(SELECTED_COMPSITE_SIM_CLASS);
    }

    /**
     * Checks if given simulation is selected
     *
     * @param sim -the simulation to check
     * @return true if selected, false otherwise
     */
    public boolean isSimulationSelected(UploadedSimulation sim) {
        El simRow = getSimRow(sim);
        String simClass = simRow.getAttribute("class");
        return simClass.contains(SELECTED_COMPSITE_SIM_CLASS);
    }

    /**
     * Checks to see if the given composite name is displayed in the table
     *
     * @param compositeName -name of the composite expected
     * @return true if displayed, false if not
     */
    public boolean isCompositeNameDisplayed(String compositeName) {
        return isElementDisplayed(getCompositeNameCell(compositeName));
    }

    /**
     * Checks to see if the name of the given simulation is displayed in the
     * table
     *
     * @param sim -simulation expected
     * @return true if displayed, false if not
     */
    public boolean isSimDisplayed(Simulation sim) {
        String simName = sim.getName();
        String simType = sim.getSimType().getLabel();
        return isSimTypeDisplayed(simName, simType);
    }

    /**
     * Checks to see if the given sim button is enabled
     *
     * @param btn the button which is being checked
     * @return true if the given button is enabled, false if it is disabled
     */
    public boolean isSimBtnEnabled(SimBtns btn) {
        String btnName = btn.getLabel();
        El simButton = el(By.xpath(SIM_BUTTON_XPATH_PREFIX + btnName + "']" + PARENT_BUTTON_STATUS_DIV_SUFFIX));
        String elClass = simButton.getAttribute(ATTR_CLASS);
        return !(elClass.contains(ATTR_DISABLED));
    }

    /**
     * Checks to see if the given configure button is enabled
     *
     * @param btn the button which is being checked
     * @return true if the given button is enabled, false if it is disabled
     */
    public boolean isConfigureBtnEnabled(SimBtns btn) {
        String btnName = btn.getLabel();
        El confBtn = el(By.xpath(SIM_BUTTON_XPATH_PREFIX + btnName + "']" + PARENT_BUTTON_STATUS_DIV_SUFFIX));
        String elClass = confBtn.getAttribute(ATTR_CLASS);
        return !(elClass.contains(ATTR_DISABLED));
    }

    /**
     * Checks if simulation template source for given simulation is displayed in
     * the same row as the name of the given simulation
     *
     * @param sim -template simulation expected
     * @return true if template source is displayed in the same row as the name;
     *         false otherwise
     */
    public boolean isSimSourceDisplayed(Simulation sim) {
        String simName = sim.getName();
        if (sim.getSimFoundation().equals(SimulationFoundation.TEMPLATE)) {
            TemplateSimulation templateSim = (TemplateSimulation)sim;
            String simSource = "Template: " + templateSim.getTemplate().getLabel();
            return isSimSourceDisplayed(simName, simSource);
        }
        else {
            return isSimSourceDisplayed(simName, NBSP_CONSTANT);
        }
    }

    /**
     * Checks if given simulation template source is displayed in the same row
     * as the given simulation name
     *
     * @param simName -template simulation name expected
     * @param simOrigin -template source (name of template) expected
     * @return true if type is displayed in the same row as the name; false
     *         otherwise
     */
    public boolean isSimSourceDisplayed(String simName, String simOrigin) {
        El row = getSimCompositeRow(simName);
        try {
            El typeCell = row.el(getCell(simOrigin));
            return typeCell.isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if Simulation header cell is displayed
     *
     * @param name -the title of the cell to find
     * @return true if cell was found, false otherwise
     */
    public boolean isCellDisplayed(ColumnName name) {
        return isElementDisplayed(getColumnTitleCell(name));
    }

    /**
     * Checks if sorting arrow icon is displayed for the given column header
     * cell
     *
     * @param name -the title of the cell where the icon is expected
     * @return true if icon was found, false otherwise
     */
    public boolean isArrowIconDisplayed(ColumnName name) {
        return isElementDisplayed(getColumnHeaderSortingArrowIcon(name));
    }

    /**
     * Checks if sorting menu is displayed
     *
     * @return true if menu was found, false if not
     */
    public boolean isSortingMenuDisplayed() {
        return isElementDisplayed(getSortingMenu());
    }

    /**
     * Checks if given sorting option is displayed
     *
     * @param option -the option expected
     * @return true if option was found, false if not
     */
    public boolean isSortingOptionDisplayed(SortingOption option) {
        return isElementDisplayed(getSortingOption(option));
    }

    /**
     * Checks if given sorting option check box is displayed
     *
     * @param option -the option check box expected
     * @return true if check box was found, false if not
     */
    public boolean isSortingItemCheckBoxDisplayed(SortingOption option) {
        return isElementDisplayed(getSortingItemCheckBox(option));
    }

    /**
     * Checks if given sorting name text box is displayed
     *
     * @return true if text box was found, false if not
     */
    public boolean isSortingNameTextBoxDisplayed() {
        return isElementDisplayed(getSortingNameTextBox());
    }

    ///////////
    //Interactions
    ///////////

    /**
     * Clicks the given button
     *
     * @param btn - the button to click
     */
    public void clickBtn(SimBtns btn) {
        getBtn(btn).click();
    }

    /**
     * Clicks the create button
     */
    public void clickCreate() {
        getBtn(SimBtns.CREATE_BTN).click();
    }

    /**
     * Clicks the delete button
     *
     * @return the newly loaded Delete Warning Form
     */
    public ConfirmDeleteModal clickDelete() {
        getBtn(SimBtns.DELETE_BTN).click();
        return getPage(ConfirmDeleteModal.class);
    }

    /**
     * Clicks the edit button
     */
    public void clickEdit() {
        getBtn(SimBtns.EDIT_BTN).click();
    }

    /**
     * Clicks the copy option displayed under Edit
     */
    public void clickCopy() {
        getEditOption(EditOptions.COPY).click();
    }

    /**
     * Clicks the export option displayed under Edit
     */
    public void clickExport() {
        getEditOption(EditOptions.EXPORT).click();
    }

    /**
     * Clicks the rename option displayed under Edit
     */
    public void clickRename() {
        getEditOption(EditOptions.RENAME).click();
    }

    /**
     * Clicks the composite settings option displayed under Edit
     *
     * @return the newly loaded CompositeSettingsModal
     */
    public CompositeSettingsModal clickCompositeSettings() {
        getEditOption(EditOptions.COMPOSITE_SETTINGS).click();
        return getPage(CompositeSettingsModal.class);
    }

    /**
     * Clicks the simulation settings option displayed under Edit
     *
     * @return the newly loaded SimulationSettingsModal
     */
    public SimulationSettingsModal clickSimulationSettings() {
        getEditOption(EditOptions.SIMULATION_SETTINGS).click();
        waitUntilLoaded();
        return getPage(SimulationSettingsModal.class);
    }

    /**
     * Clicks the reporting option displayed under Edit
     *
     * @return the newly loaded DeviceConfigurationForm
     */
    public DeviceApplicationsModal clickReporting() {
        getEditOption(EditOptions.REPORTING).click();
        waitUntilLoaded();
        return getPage(DeviceApplicationsModal.class);
    }

    /**
     * Clicks the simulation source option displayed under Edit
     */
    public void clickSimSource() {
        getEditOption(EditOptions.SIMULATION_SOURCE).click();
    }

    /**
     * Clicks the template option for a new simulation
     *
     * @return the newly loaded form to create a simulation from a template
     */
    public CreateSimulationFromTemplateModal clickTemplate() {
        getTemplateLink().click();
        waitUntilLoaded();
        return getPage(CreateSimulationFromTemplateModal.class);
    }

    /**
     * Clicks the upload option for a new simulation
     *
     * @return the newly loaded Upload a Simulation form
     */
    public CreateSimulationFromUploadModal clickUpload() {
        getUploadOption().click();
        return getPage(CreateSimulationFromUploadModal.class);
    }

    /**
     * Selects given composite based on given boolean value
     *
     * @param composite -the composite to select
     * @param selected -true if composite should be selected, false if not
     */
    public void selectComposite(CompositeSimulation composite, boolean selected) {
        String compositeName = composite.getName();
        selectSim(compositeName, selected);
    }

    /**
     * Expands the given composite
     *
     * @param composite -the composite to expand/close
     * @param expanded -true if composite should be expanded, false if it should
     *        be closed
     */
    public void expandComposite(CompositeSimulation composite, boolean expanded) {
        String compositeName = composite.getName();
        expandComposite(compositeName, expanded);
    }

    /**
     * Expands given composite
     *
     * @param compositeName -the name of the composite to select
     * @param expanded -true if composite should be expanded, false if it should
     *        be closed
     */
    public void expandComposite(String compositeName, boolean expanded) {
        El compositeRow = getSimCompositeRow(compositeName);
        El expandIcon = compositeRow.el(By.xpath(COMPOSITE_EXPAND_ICON_XPATH));
        String ariaExpanded = compositeRow.getAttribute(ARIA_EXPANDED);
        boolean isExpanded = "true".equals(ariaExpanded);
        if (expanded != isExpanded) {
            expandIcon.click();
        }
        ETexasCommonUtils.sleep(500); //ZZZ - allows time for composite to expand
    }

    /**
     * Selects given simulation based on given boolean value
     *
     * @param sim -the simulation to select
     * @param selected -true if simulation should be selected, false if not
     */
    public void selectSim(Simulation sim, boolean selected) {
        waitUntilLoaded();
        String simName = sim.getName();
        selectSim(simName, selected);
    }

    /**
     * Selects given simulation name based on given boolean value
     *
     * @param simName -the name of the simulation to select
     * @param selected -true if simulation should be selected, false if not
     */
    public void selectSim(String simName, boolean selected) {
        El simRow = getSimCompositeRow(simName);
        String rowState = simRow.getAttribute(ATTR_ARIA_SELECTED);
        boolean isSelected = rowState != null;
        if (selected != isSelected) {
            simRow.click();
        }
        ETexasCommonUtils.sleep(500); //ZZZ - allows time for buttons to become enabled/disabled following selection
    }

    /**
     * Hovers over the given column header cell and verifies sorting arrow icon
     * displays
     *
     * @param name -the title of the column header cell
     */
    public void hoverOverColumn(ColumnName name) {
        getColumnTitleCell(name).hover();
        Assert.assertTrue("Sorting arrow icon does not display when the " + name + " cell is hovered over.", isArrowIconDisplayed(name));
    }

    /**
     * Clicks the given simulation column header cell
     *
     * @param name -the title of the column header cell
     */
    public void clickColumn(ColumnName name) {
        getColumnTitleCell(name).click();
    }

    /**
     * Clicks the given header cell arrow icon
     *
     * @param name -the title of the column header cell
     */
    public void clickCellArrowIcon(ColumnName name) {
        getColumnHeaderSortingArrowIcon(name).click();
    }

    /**
     * Hovers over the Columns option in the Sorting menu
     */
    public void hoverOverColumnsOption() {
        getSortingOption(SortingOption.COLUMNS).hover();
        waitForElementToBeVisible(By.xpath(SORTING_MENU_OPTION_XPATH_PREFIX + SortingOption.NAME.getLabel() + SORTING_MENU_OPTION_XPATH_SUFFIX), 10);
    }

    /**
     * Hovers over the Columns option in the Sorting menu (clicking de-selects
     * after option has be hovered over)
     */
    public void clickColumnsOption() {
        getSortingOption(SortingOption.COLUMNS).click();
    }

    /**
     * Clicks Sort Ascending option in Sorting menu
     */
    public void clickSortAscending() {
        getSortingOption(SortingOption.SORT_ASCENDING).click();
    }

    /**
     * Clicks Sort Descending option in Sorting menu
     */
    public void clickSortDescending() {
        getSortingOption(SortingOption.SORT_DESCENDING).click();
    }

    /**
     * Selects check box for given sorting option
     *
     * @param option -the sorting option to select
     * @param checked -true if check box should be checked, false if not
     */
    public void selectSortingOptionCheckBox(SortingOption option, boolean checked) {
        El checkbox = getSortingItemCheckBox(option);
        boolean isChecked = checkbox.el(By.xpath(SELECTED_STATE_CHECKBOX_XPATH)).getAttribute(ATTR_CLASS).contains("x-menu-item-checked");
        if (checked) {
            if (!isChecked) {
                checkbox.click();
                ETexasCommonUtils.sleep(1000);
            }
        }
        else if (!checked) {
            if (isChecked) {
                checkbox.click();
                ETexasCommonUtils.sleep(1000);
            }
        }
    }

    /**
     * Enters the name of the given simulation in the sorting name text box
     *
     * @param simulation -the simulation to search for
     */
    public void searchForSimulation(Simulation simulation) {
        String name = simulation.getName();
        getSortingNameTextBox().setText(name);
        ETexasCommonUtils.sleep(500); //ZZZ - search box filters as text is entered, sleep is in place to ensure filtering is completed before checking results
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Check if simulations header displays the text 'Simulation Configurations'
     */
    public void checkSimulationsHeaderText() {
        String displayedText = getSimulationsHeader().getText();
        Assert.assertEquals("Simulations header is not displaying the exepected text, the text '" + displayedText + "' is displayed.", SIMULATIONS_HEADER_TEXT, displayedText);
    }

    /**
     * Check if all simulation configuration buttons are displayed
     */
    public void checkAllSimulationBtns() {
        Assert.assertTrue("Create button is not displayed as expected.", isBtnDisplayed(SimBtns.CREATE_BTN));
        Assert.assertTrue("Edit button is not displayed as expected.", isBtnDisplayed(SimBtns.EDIT_BTN));
        Assert.assertTrue("Delete simulation button is not displayed as expected.", isBtnDisplayed(SimBtns.DELETE_BTN));
    }

    /**
     * Checks that the Edit and Delete Simulation buttons are disabled
     */
    public void checkDisabledSimulationBtns() {
        //Assert.assertFalse("The Edit button is enabled.", isBtnEnabled(SimBtns.EDIT_BTN)); BUG 13345
        Assert.assertFalse("The Delete button is enabled.", isBtnEnabled(SimBtns.DELETE_BTN));
    }

    /**
     * Checks that the Create, Edit and Delete buttons are enabled
     */
    public void checkEnabledSimulationBtns() {
        Assert.assertTrue("The Create button is disabled.", isBtnEnabled(SimBtns.CREATE_BTN));
        Assert.assertTrue("The Edit button is disabled.", isBtnEnabled(SimBtns.EDIT_BTN));
        Assert.assertTrue("The Delete button is disabled.", isBtnEnabled(SimBtns.DELETE_BTN));
    }

    /**
     * Checks that the Template and Upload options are present when the Create
     * button is clicked
     */
    public void checkCreateSimOptions() {
        Assert.assertTrue("The template option is not displayed.", isTemplateOptionDisplayed());
        Assert.assertTrue("The upload option is not displayed.", isUploadOptionDisplayed());
    }

    /**
     * Checks details of simulation displayed in table (Name, Origin(Template),
     * Type)
     *
     * @param sim the simulation which is being checked for display in the table
     */
    public void checkSimulationDetails(TemplateSimulation sim) {
        Assert.assertTrue("Simulation name could not be found.", isSimDisplayed(sim));
        Assert.assertTrue("Simulation type was not found in row with simulation name.", isSimTypeDisplayed(sim));
        Assert.assertTrue("Simulation template source (name of template) was not found in row with simulation name.", isSimSourceDisplayed(sim));
    }

    /**
     * Checks for presence of all options displayed under Edit
     */
    public void checkEditOptions() {
        Assert.assertTrue("Copy option could not be found.", isEditOptionDisplayed(EditOptions.COPY));
        Assert.assertTrue("Export option could not be found.", isEditOptionDisplayed(EditOptions.EXPORT));
        Assert.assertTrue("Rename option could not be found.", isEditOptionDisplayed(EditOptions.RENAME));
        Assert.assertTrue("Composite Settings option could not be found.", isEditOptionDisplayed(EditOptions.COMPOSITE_SETTINGS));
        Assert.assertTrue("Simulation Settings option could not be found.", isEditOptionDisplayed(EditOptions.SIMULATION_SETTINGS));
        Assert.assertTrue("Reporting option could not be found.", isEditOptionDisplayed(EditOptions.REPORTING));
        Assert.assertTrue("Simulation Source option could not be found.", isEditOptionDisplayed(EditOptions.SIMULATION_SOURCE));
    }

    /**
     * Checks for expected enabled options when a simulation has been selected
     */
    public void checkEditOptionsEnabledWithSimSelected() {
        Assert.assertTrue("Copy option not enabled when simulation selected.", isEditOptionEnabled(EditOptions.COPY));
        Assert.assertTrue("Export option not enabled when simulation selected.", isEditOptionEnabled(EditOptions.EXPORT));
        Assert.assertTrue("Rename option not enabled when simulation selected.", isEditOptionEnabled(EditOptions.RENAME));
        Assert.assertFalse("Composite Settings not disabled when simulation selected.", isEditOptionEnabled(EditOptions.COMPOSITE_SETTINGS));
        Assert.assertTrue("Simulation Settings not enabled when simulation selected.", isEditOptionEnabled(EditOptions.SIMULATION_SETTINGS));
        Assert.assertFalse("Reporting option not disabled when simulation selected.", isEditOptionEnabled(EditOptions.REPORTING));
        Assert.assertTrue("Simulation Source not enabled when simulation selected.", isEditOptionEnabled(EditOptions.SIMULATION_SOURCE));
    }

    /**
     * Checks for expected enabled options when a composite has been selected
     */
    public void checkEditOptionsEnabledWithCompositeSelected() {
        Assert.assertTrue("Copy option not enabled when composite selected.", isEditOptionEnabled(EditOptions.COPY));
        Assert.assertTrue("Export option not enabled when composite selected.", isEditOptionEnabled(EditOptions.EXPORT));
        Assert.assertTrue("Rename option not enabled when composite selected.", isEditOptionEnabled(EditOptions.RENAME));
        Assert.assertTrue("Composite Settings not enabled when composite selected.", isEditOptionEnabled(EditOptions.COMPOSITE_SETTINGS));
        Assert.assertFalse("Simulation Settings not disabled when composite selected.", isEditOptionEnabled(EditOptions.SIMULATION_SETTINGS));
        Assert.assertTrue("Reporting option not enabled when composite selected.", isEditOptionEnabled(EditOptions.REPORTING));
        Assert.assertFalse("Simulation Source not disabled when composite selected.", isEditOptionEnabled(EditOptions.SIMULATION_SOURCE));
    }

    /**
     * Checks that the Template and Upload options are present (options display
     * after the Create buttons is clicked)
     */
    public void checkCreateOptions() {
        Assert.assertTrue("The template option is not displayed.", isTemplateOptionDisplayed());
        Assert.assertTrue("The upload option is not displayed.", isUploadOptionDisplayed());
    }

    /**
     * Checks for presence of sorting options in sorting menu (Sort Ascending,
     * Sort Descending, and Columns)
     */
    public void checkSortingOptions() {
        Assert.assertTrue("Sort Ascending sorting option could not be found.", isSortingOptionDisplayed(SortingOption.SORT_ASCENDING));
        Assert.assertTrue("Sort Descending sorting option could not be found.", isSortingOptionDisplayed(SortingOption.SORT_DESCENDING));
        Assert.assertTrue("Columns sorting option could not be found.", isSortingOptionDisplayed(SortingOption.COLUMNS));
    }

    /**
     * Checks for presence of column sub-options in sorting menu (ID, Name,
     * Type, and Source)
     */
    public void checkColumnOptions() {
        Assert.assertTrue("ID sub-option could not be found.", isSortingOptionDisplayed(SortingOption.ID));
        Assert.assertTrue("Name sub-option could not be found.", isSortingOptionDisplayed(SortingOption.NAME));
        Assert.assertTrue("Status sub-option could not be found.", isSortingOptionDisplayed(SortingOption.TYPE));
        Assert.assertTrue("Date Created sub-option could not be found.", isSortingOptionDisplayed(SortingOption.SOURCE));
    }

    //////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(SIMULATIONS_HEADER_XPATH), 20);
    }
}
