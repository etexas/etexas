package com.harmonia.qa.ETEXASWebQATests.webdriver.pages;

import java.util.List;

import junit.framework.Assert;

import org.openqa.selenium.By;
import org.openqa.selenium.NoSuchElementException;
import org.openqa.selenium.WebDriver;

import com.harmonia.qa.ETEXASWebQATests.enums.ExecutionDetailsTabs;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage.ColumnName;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.FilterOptionsMenu;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.MessagesHelpWindow;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DetectorsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.DevicesHelpWindow;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings.ViewDeviceWindow;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.CommandQueueOptionsHelpWindow;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.InjectVehicleForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.LaneChangeForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.SignalChangeForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.SignalsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.SpeedChangeForm;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.VehiclesPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.executions.ViewingMessageWindow;
import com.harmonia.qa.webdriver.utilities.elements.El;

/**
 * Page class representing the eTexas Executions Page
 *
 * @author llaroussini
 */
public class ExecutionsPage extends CompletedExecutionPage {

    /**
     * Default constructor
     *
     * @param driver the web driver instance being used
     */
    public ExecutionsPage(WebDriver driver) {
        super(driver);
        waitUntilLoaded();
    }

    /**
     * Buttons associated with stepping through executions on executions page
     *
     * @author llaroussini
     */
    public enum ExecutionControlBtns {
        /**
         * The Next Step button associated with controlling an execution
         */
        NEXT_STEP_BTN("Next Step"),
        /**
         * The Finish button associated with controlling an execution
         */
        FINISH_BTN("Finish"),
        /**
         * The View Animation button associated with controlling an execution
         */
        VIEW_ANIMATION_BTN("View Animation");

        /**
         * The label of the buttons as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        ExecutionControlBtns(String label) {
            this.label = label;
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
    }

    /**
     * Section headers displayed on Executions Page
     *
     * @author llaroussini
     */
    public enum SectionHeaders {
        /**
         * The Command Queue section header
         */
        COMMAND_QUEUE("Command Queue"),
        /**
         * The Messages section header
         */
        MESSAGES("Messages"),
        /**
         * The Devices section header
         */
        DEVICES("Devices");

        /**
         * The label of the headers as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        SectionHeaders(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the header as it is displayed in the
         * Web UI
         *
         * @return The label of the option
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * Options listed under Commands in Command Queue section
     *
     * @author llaroussini
     */
    public enum CommandOptions {
        /**
         * The Speed Change option
         */
        SPEED_CHANGE("Speed Change"),
        /**
         * The Lane Change option
         */
        LANE_CHANGE("Lane Change"),
        /**
         * The Signal Change option
         */
        SIGNAL_CHANGE("Signal Change");

        /**
         * The label of the options as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        CommandOptions(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the option as it is displayed in the
         * Web UI
         *
         * @return The label of the option
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * Column headers listed in Messages section
     *
     * @author llaroussini
     */
    public enum MessageColumns {
        /**
         * The ID column
         */
        ID("Id"),
        /**
         * The Type column
         */
        TYPE("Type"),
        /**
         * The Actions column
         */
        ACTIONS("Actions");

        /**
         * The label of the column headers as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        MessageColumns(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the column header as it is displayed
         * in the Web UI
         *
         * @return The label of the option
         */
        public String getLabel() {
            return this.label;
        }
    }

    /**
     * Column headers listed in Devices section
     *
     * @author llaroussini
     */
    public enum DevicesColumns {
        /**
         * The ID column
         */
        ID("Id"),
        /**
         * The Name column
         */
        NAME("Name"),
        /**
         * The Percentage column
         */
        PERCENTAGE("Percentage");

        /**
         * The label of the column headers as they appear in the application
         */
        private String label;

        /**
         * Default constructor; sets the label
         *
         * @param label The string to set as the label
         */
        DevicesColumns(String label) {
            this.label = label;
        }

        /**
         * Gets the label associated with the column header as it is displayed
         * in the Web UI
         *
         * @return The label of the option
         */
        public String getLabel() {
            return this.label;
        }
    }

    ///////////
    //Element ID's & Identifiers
    ///////////

    /**
     * Xpath to the Executions controls toolbar
     */
    private static final String EXECUTION_CONTROLS_TOOLBAR_XPATH = "//div[contains(@id, 'executionViewer')]//div[contains(@id, 'toolbar')]";

    /**
     * Xpath assigned to the area displaying the sim time
     */
    private static final String SIM_TIME_AREA_XPATH = "//span[text()='Sim Time:']/ancestor::div[contains(@class, 'x-hbox-form-item')]";

    /**
     * XPath assigned to the Steps text box
     */
    private static final String STEPS_TEXT_BOX_XPATH = "//input[contains(@class, 'x-form-field')][@role='spinbutton']";

    /**
     * Xpath assigned to the area displaying the remaining steps
     */
    private static final String REMAINING_STEPS_AREA_XPATH = "//span[text()='Remaining:']/ancestor::div[contains(@class, 'x-hbox-form-item')]";

    /**
     * Xpath to get the value displayed in an area (i.e., value of sim time or
     * remaining steps)
     */
    private static final String VALUE_XPATH = ".//div[contains(@class, 'x-form-display-field-default')]";

    /**
     * Xpath prefix for buttons
     */
    private static final String BTN_XPATH_PREFIX = EXECUTION_CONTROLS_TOOLBAR_XPATH + "//a[contains(@class, 'x-btn-default-toolbar-small')]//span[text()='";

    /**
     * Xpath suffix for buttons
     */
    private static final String BTN_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath prefix for tabs
     */
    private static final String TAB_XPATH_PREFIX = "//span[contains(@class, 'x-tab-inner-default')][text()='";

    /**
     * Xpath suffix for tabs
     */
    private static final String TAB_XPATH_SUFFIX = "']/ancestor::a";

    /**
     * Xpath prefix to specific section on executions page (Command Queue,
     * Messages, Devices)
     */
    private static final String SECTION_XPATH_PREFIX = "//div[text()='";

    /**
     * Xpath suffix to to specific section header on executions page (Command
     * Queue, Messages, Devices)
     */
    private static final String SECTION_HEADER_XPATH_SUFFIX = "']/ancestor::div[contains(@class, 'x-accordion-hd')]";

    /**
     * Xpath suffix to specific section on executions page (Command Queue,
     * Messages, Devices)
     */
    private static final String SECTION_XPATH_SUFFIX = "']/ancestor::div[contains(@class, 'x-accordion-item')]";

    /**
     * Xpath suffix to specific section table on executions page (Command Queue,
     * Messages, Devices)
     */
    private static final String SECTION_TABLE_XPATH_SUFFIX = "']/ancestor::div[contains(@class, 'x-accordion-item')]//table";

    /**
     * The xpath for the help icon
     */
    private static final String HELP_ICON_XPATH = ".//img[contains(@class, 'x-tool-help')]";

    /**
     * The xpath for the minimize icon
     */
    private static final String MINIMIZE_ICON_XPATH = ".//img[contains(@class, 'x-tool-collapse-top')]";

    /**
     * The xpath for the maximize icon
     */
    private static final String MAXIMIZE_ICON_XPATH = ".//img[contains(@class, 'x-tool-expand-bottom')]";

    /**
     * Xpath to the Commands button
     */
    private static final String COMMANDS_BTN_XPATH = "//span[@data-ref='btnInnerEl'][text()='Commands']";

    /**
     * Xpath to the Inject Vehicle button
     */
    private static final String INJECT_VEHICLE_BTN_XPATH = "//span[@data-ref='btnInnerEl'][text()='Inject Vehicle']";

    /**
     * Xpath prefix to the command options (listed when commands button is
     * clicked)
     */
    private static final String COMMAND_OPTIONS_XPATH_PREFIX = "//span[@data-ref='textEl'][text()='";

    /**
     * Xpath suffix to the command options (listed when commands button is
     * clicked)
     */
    private static final String COMMAND_OPTIONS_XPATH_SUFFIX = "']";

    /**
     * Xpath prefix to specific row in command queue
     */
    private static final String COMMAND_QUEUE_ROW_XPATH_PREFIX = SECTION_XPATH_PREFIX + SectionHeaders.COMMAND_QUEUE.label + SECTION_TABLE_XPATH_SUFFIX + "//div[contains(text(), '";

    /**
     * Xpath to Messages table (in messages section)
     */
    private static final String MESSAGES_TABLE_XPATH = SECTION_XPATH_PREFIX + SectionHeaders.MESSAGES.label + SECTION_XPATH_SUFFIX;

    /**
     * Xpath to Devices table (in devices section)
     */
    private static final String DEVICES_TABLE_XPATH = SECTION_XPATH_PREFIX + SectionHeaders.DEVICES.label + SECTION_TABLE_XPATH_SUFFIX;

    /**
     * Xpath to all rows displayed in Messages table
     */
    private static final String MESSAGES_ROW_XPATH = MESSAGES_TABLE_XPATH + "//table";

    /**
     * Xpath to generic BSMV row in messages section
     */
    private static final String BSMV_MESSAGE_ROW_XPATH = MESSAGES_TABLE_XPATH + "//div[contains(text(), 'BSMV')]/ancestor::table";

    /**
     * Xpath to generic SPAT row in messages section
     */
    private static final String SPAT_MESSAGE_ROW_XPATH = MESSAGES_TABLE_XPATH + "//div[contains(text(), 'SPAT')]/ancestor::table";

    /**
     * Xpath to generic device row in devices section
     */
    private static final String DEVICE_ROW_XPATH = DEVICES_TABLE_XPATH + "//div[contains(text(), 'Device')]/ancestor::tr";

    /**
     * Xpath to specific row in messages section
     */
    private static final String SPECIFIC_MESSAGE_ROW_XPATH_PREFIX = MESSAGES_TABLE_XPATH + "//div[text()='";

    /**
     * Xpath to specific device in devices section
     */
    private static final String DEVICE_SPECIFIC_MESSAGE_ROW_XPATH_PREFIX = DEVICES_TABLE_XPATH + "//div[text()='";

    /**
     * Xpath suffix to specific row
     */
    private static final String ROW_XPATH_SUFFIX = "')]/ancestor::tr";

    /**
     * By of Next Step loading alert (displayed after Next Step button clicked)
     */
    private static final By NEXT_STEP_LOADING_ALERT_BY = By.xpath("//div[contains(@id, 'executionViewer')][contains(@class, 'x-box-item')]/div[4]//div[text()='Loading...']"); //TODO update to more stable xpath when unique identifiers are implemented

    /**
     * Xpath prefix to specific column in command queue, messages, or devices
     * sections (should be preceded with section in which the column is
     * expected)
     */
    private static final String COLUMN_XPATH_PREFIX = ".//span[text()='";

    /**
     * Xpath suffix to specific column dropdown selector in command queue,
     * messages, or devices sections
     */
    private static final String COLUMN_DROPDOWN_XPATH_SUFFIX = "']//ancestor::div[contains(@class, 'x-leaf-column-header')]//div[@data-ref='triggerEl']";

    /**
     * Xpath prefix to info icon in messages/devices sections
     */
    private static final String INFO_ICON_XPATH_SUFFIX = ".//td//img[@role='button']";

    /**
     * Text displayed in tool tip for info icons in Messages section
     */
    private static final String MESSAGES_INFO_ICON_TOOL_TIP_TEXT = "View message content";

    /**
     * Text displayed in tool tip for info icons in Devices section
     */
    private static final String DEVICES_INFO_ICON_TOOL_TIP_TEXT = "View device apps";

    /**
     * Xpath prefix to Message column arrow icon
     */
    private static final String MESSAGES_COLUMN_ARROW_ICON_XPATH_PREFIX = "//div[contains(@id, 'messagesViewer')]//span[text()='";

    /**
     * Xpath suffix to Message column ascending arrow icon
     */
    private static final String MESSAGES_COLUMN_ARROW_ASCENDING_ICON_XPATH_SUFFIX = "']//ancestor::div[contains(@class, 'x-column-header-sort-ASC']";

    /**
     * Xpath suffic to Message column descending arrow icon
     */
    private static final String MESSAGES_COLUMN_ARROW_DESCENDING_ICON_XPATH_SUFFIX = "']//ancestor::div[contains(@class, 'x-column-header-sort-DESC']";

    /**
     * Xpath to ID cell for message row (use in conjuction with row element)
     */
    private static final String MESSAGE_ID_CELL = ".//td[1]"; //TODO update when unique identifier is in place

    /**
     * Steps field name as displayed in UI
     */
    private static final String STEPS_FIELD_DISPLAYED_NAME = "Steps";

    /**
     * Error text displayed with icon/tooltip when invalid, non-numeric, steps
     * value is used
     */
    private static final String INVALID_NON_NUMERIC_STEPS_VALUE_ERROR_TEXT = "is not a valid number";

    /**
     * Error text displayed with icon/tooltip when steps value exceeding the
     * maximum is entered
     */
    private static final String STEPS_EXCEEDING_MAX_ERROR_TEXT = "The maximum value for this field is";

    /**
     * Xpath prefix to the area displaying the simulation name for the execution
     * being viewed
     */
    private static final String DISPLAYED_SIMULATION_NAME_XPATH = "//label//span[text()='Simulation:']/ancestor::div[contains(@id, 'displayfield')]//div[contains(@id, 'inputEl')]";

    /**
     * Xpath prefix to the title cell for the messages column in side panel
     */
    private static final String MESSAGES_COLUMN_TITLE_CELL_XPATH_PREFIX = "//div[contains(@id, 'messagesViewer')][contains(@class, 'x-accordion-body')]//span[contains(@id, 'gridcolumn')][text()='";

    /**
     * Xpath of the Sorting Arrow icon displayed in column header cells
     */
    private static final String SORTING_ARROW_ICON_XPATH = "./ancestor::div[contains(@class, 'x-column-header-inner')]//div[@class='x-column-header-trigger']";

    //Element Getters
    ///////////

    /**
     * Gets the given execution control button element
     *
     * @param execBtn -the button desired
     * @return the button element
     */
    private El getExecutionControlBtn(ExecutionControlBtns execBtn) {
        return el(By.xpath(BTN_XPATH_PREFIX + execBtn.label + BTN_XPATH_SUFFIX));
    }

    /**
     * Gets the Steps text box
     *
     * @return the text box element
     */
    private El getStepsTextBox() {
        return el(By.xpath(STEPS_TEXT_BOX_XPATH));
    }

    /**
     * Gets the Sim Time area
     *
     * @return the area element
     */
    private El getSimTimeArea() {
        return el(By.xpath(SIM_TIME_AREA_XPATH));
    }

    /**
     * Gets the Remaining Steps area
     *
     * @return the area element
     */
    private El getRemainingStepsArea() {
        return el(By.xpath(REMAINING_STEPS_AREA_XPATH));
    }

    /**
     * Gets the given details tab element
     *
     * @param tab -the details tab to get
     * @return the tab element
     */
    private El getDetailsTab(ExecutionDetailsTabs tab) {
        return el(By.xpath(TAB_XPATH_PREFIX + tab.getLabel() + TAB_XPATH_SUFFIX));
    }

    /**
     * Gets the value displayed as the sim time
     *
     * @return the string value of sim time displayed
     */
    public String getSimTime() {
        El simValueEl = getSimTimeArea().el(By.xpath(VALUE_XPATH));
        return simValueEl.getText();
    }

    /**
     * Gets the value displayed as the remaining steps
     *
     * @return the string value of remaining steps displayed
     */
    public String getRemainingSteps() {
        El stepsValueEl = getRemainingStepsArea().el(By.xpath(VALUE_XPATH));
        return stepsValueEl.getText();
    }

    /**
     * Gets the given section header
     *
     * @param header -the section header to get
     * @return the section header element
     */
    private El getSectionHeader(SectionHeaders header) {
        return el(By.xpath(SECTION_XPATH_PREFIX + header.label + SECTION_HEADER_XPATH_SUFFIX));
    }

    /**
     * Gets the Command Queue section
     *
     * @param header -the header of the section to get
     * @return the section element
     */
    private El getSection(SectionHeaders header) {
        return el(By.xpath(SECTION_XPATH_PREFIX + header.label + SECTION_XPATH_SUFFIX));
    }

    /**
     * The xpath to get the help icon from the header
     *
     * @param header -the header where icon is expected
     * @return the help icon
     */
    private El getHelpIcon(El header) {
        return header.el(By.xpath(HELP_ICON_XPATH));
    }

    /**
     * The xpath to get the minimize icon from the header
     *
     * @param header -the header where icon is expected
     * @return the minimize icon
     */
    private El getMinimizeIcon(El header) {
        return header.el(By.xpath(MINIMIZE_ICON_XPATH));
    }

    /**
     * The xpath to get the maximize icon from the header
     *
     * @param header -the header where icon is expected
     * @return the maximize icon
     */
    private El getMaximizeIcon(El header) {
        return header.el(By.xpath(MAXIMIZE_ICON_XPATH));
    }

    /**
     * Gets the Commands button
     *
     * @return the button element
     */
    private El getCommandsBtn() {
        return el(By.xpath(COMMANDS_BTN_XPATH));
    }

    /**
     * Gets the Inject Vehicle button
     *
     * @return the button element
     */
    private El getInjectVehicleBtn() {
        return el(By.xpath(INJECT_VEHICLE_BTN_XPATH));
    }

    /**
     * Gets the given command option (listed under Commands)
     *
     * @param option -the option to get
     * @return the option element
     */
    private El getCommandOption(CommandOptions option) {
        String label = option.getLabel();
        return el(By.xpath(COMMAND_OPTIONS_XPATH_PREFIX + label + COMMAND_OPTIONS_XPATH_SUFFIX));
    }

    /**
     * Gets the command queue table
     *
     * @param header -the header of the section table to get
     * @return the table element
     */
    private El getSectionTable(SectionHeaders header) {
        return el(By.xpath(SECTION_XPATH_PREFIX + header.label + SECTION_TABLE_XPATH_SUFFIX));
    }

    /**
     * Gets the list of commands displayed in command queue
     *
     * @return the list of commands
     */
    private List<El> getCommandQueueList() {
        return getSectionTable(SectionHeaders.COMMAND_QUEUE).els(By.xpath(".//tr"));
    }

    /**
     * Gets specific row in command queue list containing given text
     *
     * @param text -the text expected in the command
     * @return the command row element
     */
    private El getCommandQueueRow(String text) {
        return el(By.xpath(COMMAND_QUEUE_ROW_XPATH_PREFIX + text + ROW_XPATH_SUFFIX));
    }

    /**
     * Gets all rows in the Messages table
     *
     * @return the Message table rows
     */
    private List<El> getMessageRows() {
        return els(By.xpath(MESSAGES_ROW_XPATH));
    }

    /**
     * Gets all BSMV rows in message list
     *
     * @return the row elements
     */
    private List<El> getBSMVMessageRows() {
        return els(By.xpath(BSMV_MESSAGE_ROW_XPATH));
    }

    /**
     * Gets all SPAT rows in message list
     *
     * @return the row elements
     */
    private List<El> getSPATMessageRows() {
        return els(By.xpath(SPAT_MESSAGE_ROW_XPATH));
    }

    /**
     * Gets all device rows in devices section
     *
     * @return the row elements
     */
    private List<El> getDeviceRows() {
        return els(By.xpath(DEVICE_ROW_XPATH));
    }

    /**
     * Gets the first displayed BSMV message
     *
     * @return the row element
     */
    private El getFirstBSMVMessageRow() {
        return el(By.xpath(BSMV_MESSAGE_ROW_XPATH));
    }

    /**
     * Gets the BSMV message row with the given id
     *
     * @param id -the row id
     * @return the row element
     */
    private El getSpecificMessageRow(String id) {
        return el(By.xpath(SPECIFIC_MESSAGE_ROW_XPATH_PREFIX + id + "']/ancestor::tr"));
    }

    /**
     * Gets the info icon in the first displayed BSMV row
     *
     * @param id -the id of the message row
     * @return the info icon element
     */
    private El getSpecificMessageInfoIcon(String id) {
        return getSpecificMessageRow(id).el(By.xpath(INFO_ICON_XPATH_SUFFIX));
    }

    /**
     * Gets the id value for the first displayed BSMV message
     *
     * @return the id as string
     */
    public String getFirstBSMVMessageId() {
        El idCell = getFirstBSMVMessageRow().el(By.xpath(".//td[1]")); //TODO update when unique identifiers are in place
        return idCell.getText();
    }

    /**
     * Gets the number of BSMV messages in Messages section
     *
     * @return the number of BSMV messages
     */
    public int getCountOfBSMVMessages() {
        return getBSMVMessageRows().size();
    }

    /**
     * Gets the first displayed SPAT message
     *
     * @return the row element
     */
    private El getFirstSPATMessageRow() {
        return el(By.xpath(SPAT_MESSAGE_ROW_XPATH));
    }

    /**
     * Gets the id value for the first displayed BSMV message
     *
     * @return the id as string
     */
    public String getFirstSPATMessageId() {
        El idCell = getFirstSPATMessageRow().el(By.xpath(".//td[1]")); //TODO update when unique identifiers are in place
        return idCell.getText();
    }

    /**
     * Gets the number of SPAT messages in Messages section
     *
     * @return the number of SPAT messages
     */
    public int getCountOfSPATMessages() {
        return getSPATMessageRows().size();
    }

    /**
     * Gets the given column header in the Messages section
     *
     * @param column -the column expected
     * @return the column header element
     */
    private El getMessagesColumn(MessageColumns column) {
        return getSection(SectionHeaders.MESSAGES).el(By.xpath(COLUMN_XPATH_PREFIX + column.label + "']"));
    }

    /**
     * Gets the drop down for the given column header in the Devices section
     *
     * @param column -the column expected
     * @return the column drop down element
     */
    private El getMessagesColumnDropdown(MessageColumns column) {
        return getSection(SectionHeaders.MESSAGES).el(By.xpath(COLUMN_XPATH_PREFIX + column.label + COLUMN_DROPDOWN_XPATH_SUFFIX));
    }

    /**
     * Gets the given column header in the Devices section
     *
     * @param column -the column expected
     * @return the column header element
     */
    private El getDevicesColumn(DevicesColumns column) {
        return getSection(SectionHeaders.DEVICES).el(By.xpath(COLUMN_XPATH_PREFIX + column.label + "']"));
    }

    /**
     * Gets the row of the given device
     *
     * @param deviceName -the name of the device to get
     * @return the row element
     */
    private El getDeviceRow(String deviceName) {
        return el(By.xpath(DEVICE_SPECIFIC_MESSAGE_ROW_XPATH_PREFIX + deviceName + "']/ancestor::tr"));
    }

    /**
     * Gets the info icon in the given device
     *
     * @param deviceName -the name of the device
     * @return the info icon element
     */
    private El getDeviceInfoIcon(String deviceName) {
        return getDeviceRow(deviceName).el(By.xpath(INFO_ICON_XPATH_SUFFIX));
    }

    /**
     * Gets the ascending arrow icon associated with the given column header in
     * Messages section
     *
     * @param column -the header with ascending arrow icon
     * @return the ascending arrow icon
     */
    private El getMessageColumnAscendingArrowIcon(MessageColumns column) {
        return el(By.xpath(MESSAGES_COLUMN_ARROW_ICON_XPATH_PREFIX + column.label + MESSAGES_COLUMN_ARROW_ASCENDING_ICON_XPATH_SUFFIX));
    }

    /**
     * Gets the descending arrow icon associated with the given column header in
     * Messages section
     *
     * @param column -the header with descending arrow icon
     * @return the descending arrow icon
     */
    private El getMessageColumnDescendingArrowIcon(MessageColumns column) {
        return el(By.xpath(MESSAGES_COLUMN_ARROW_ICON_XPATH_PREFIX + column.label + MESSAGES_COLUMN_ARROW_DESCENDING_ICON_XPATH_SUFFIX));
    }

    /**
     * Gets message ID cell for given row
     *
     * @param row -the row in which the ID cell is needed
     * @return the ID cell element
     */
    private El getMessageIDCell(El row) {
        return row.el(By.xpath(MESSAGE_ID_CELL));
    }

    /**
     * Gets the displayed simulation name area
     *
     * @return the area displaying the simulation name
     */
    private El getDisplayedSimName() {
        return el(By.xpath(DISPLAYED_SIMULATION_NAME_XPATH));
    }

    /**
     * Gets the title cell for a column in the messages panel
     *
     * @param name -the title of the cell
     * @return the cell element
     */
    private El getMessagesColumnTitleCell(ColumnName name) {
        String displayedName = name.getLabel();
        return el(By.xpath(MESSAGES_COLUMN_TITLE_CELL_XPATH_PREFIX + displayedName + "']"));
    }

    /**
     * Gets the sorting arrow icon displayed in given Messages column header
     * cell
     *
     * @param name -the title of the column where the sorting arrow icon is
     *        expected
     * @return the icon element
     */
    private El getMessagesColumnHeaderSortingArrowIcon(ColumnName name) {
        return getMessagesColumnTitleCell(name).el(By.xpath(SORTING_ARROW_ICON_XPATH));
    }

    ///////////
    //Element Checkers
    ///////////

    /**
     * Checks if Sim Time area is displayed
     *
     * @return true if displayed, false if cannot be found or is not visible
     */
    public boolean isSimTimeAreaDisplayed() {
        return isElementDisplayed(getSimTimeArea());
    }

    /**
     * Checks if Steps text box is displayed
     *
     * @return true if displayed, false if cannot be found or is not visible
     */
    public boolean isStepsTextBoxDisplayed() {
        return isElementDisplayed(getStepsTextBox());
    }

    /**
     * Checks if Remaining Steps area is displayed
     *
     * @return true if displayed, false if cannot be found or is not visible
     */
    public boolean isRemaininsStepsAreDisplayed() {
        return isElementDisplayed(getRemainingStepsArea());
    }

    /**
     * Checks if given execution control button is displayed
     *
     * @param execBtn -the button expected
     * @return true if displayed, false if cannot be found or is not visible
     */
    public boolean isExeuctionControlBtnDisplayed(ExecutionControlBtns execBtn) {
        return isElementDisplayed(getExecutionControlBtn(execBtn));
    }

    /**
     * Checks if the given execution details tab is displayed
     *
     * @param tab -the tab expected
     * @return true if displayed, false if cannot be found or is not visible
     */
    public boolean isActiveExecTabDisplayed(ExecutionDetailsTabs tab) {
        return isElementDisplayed(getDetailsTab(tab));
    }

    /**
     * Checks if section header is displayed
     *
     * @param header -the header of the section expected
     * @return true if displayed, false otherwise
     */
    public boolean isSectionHeaderDisplayed(SectionHeaders header) {
        return isElementDisplayed(getSectionHeader(header));
    }

    /**
     * Checks if section is displayed
     *
     * @param header -the header of the section expected
     * @return true if displayed, false otherwise
     */
    public boolean isSectionDisplayed(SectionHeaders header) {
        return isElementDisplayed(getSection(header));
    }

    /**
     * Checks to see if the help icon is displayed in section header
     *
     * @param header -the header of the section where help icon is expected
     * @return true if the help icon is displayed, false if it is not or cannot
     *         be found
     */
    public boolean isSectionHelpIconDisplayed(SectionHeaders header) {
        return isElementDisplayed(getHelpIcon(getSectionHeader(header)));
    }

    /**
     * Checks to see if the minimize icon is displayed in section header
     *
     * @param header -the header of the section where minimize icon is expected
     * @return true if the minimize icon is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isMinimizeIconDisplayed(SectionHeaders header) {
        return isElementDisplayed(getMinimizeIcon(getSectionHeader(header)));
    }

    /**
     * Checks to see if the maximize icon is displayed in section header
     *
     * @param header -the header of the section where maximize icon is expected
     * @return true if the maximize icon is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isMaximizeIconDisplayed(SectionHeaders header) {
        return isElementDisplayed(getMaximizeIcon(getSectionHeader(header)));
    }

    /**
     * Checks if section header is collapsed
     *
     * @param header -the header of the section to check if collapsed
     * @return true if collapsed, false if expanded
     */
    public boolean isSectionHeaderCollapsed(SectionHeaders header) {
        return getSectionHeader(header).hasClass("x-collapsed");
    }

    /**
     * Checks if Commands button is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isCommandsBtnDisplayed() {
        return isElementDisplayed(getCommandsBtn());
    }

    /**
     * Checks if Inject Vehicle button is displayed
     *
     * @return true if displayed, false otherwise
     */
    public boolean isInjectVehicleBtnDisplayed() {
        return isElementDisplayed(getInjectVehicleBtn());
    }

    /**
     * Checks if the given command option is displayed
     *
     * @param option -the option expected
     * @return true if displayed, false otherwise
     */
    public boolean isCommandOptionDisplayed(CommandOptions option) {
        return isElementDisplayed(getCommandOption(option));
    }

    /**
     * Check if Command Queue list is displayed
     *
     * @return true if list is not empty, false otherwise
     */
    public boolean isCommandQueueListDisplayed() {
        try {
            return !(getCommandQueueList().isEmpty());
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if command row is displayed containing the given text
     *
     * @param text -the text expected
     * @return true if displayed, false otherwise
     */
    public boolean isCommandQueueRowDisplayed(String text) {
        try {
            El row = getCommandQueueRow(text);
            return row != null && row.isDisplayed();
        }
        catch (NoSuchElementException e) {
            return false;
        }
    }

    /**
     * Checks if messages column header is displayed
     *
     * @param column -the column expected
     * @return true if displayed, false otherwise
     */
    public boolean isMessagesColumnHeaderDisplayed(MessageColumns column) {
        String style = getMessagesColumn(column).getAttribute("style");
        return !(style.contains("display: none;"));
    }

    /**
     * Checks if devices column header is displayed
     *
     * @param column -the column expected
     * @return true if displayed, false otherwise
     */
    public boolean isDevicesColumnHeaderDisplayed(DevicesColumns column) {
        return isElementDisplayed(getDevicesColumn(column));
    }

    /**
     * Checks if devices row is displayed
     *
     * @param deviceName -the name of the device expected
     * @return true if displayed, false otherwise
     */
    public boolean isDeviceRowDisplayed(String deviceName) {
        return isElementDisplayed(getDeviceRow(deviceName));
    }

    /**
     * Checks for display of ascending arrow icon in given header in Messages
     * section
     *
     * @param column -the header column in messages section when icon is
     *        expected
     * @return true if displayed, false otherwise
     */
    public boolean isMessagesAscendingArrowDisplayed(MessageColumns column) {
        return isElementDisplayed(getMessageColumnAscendingArrowIcon(column));
    }

    /**
     * Checks for display of descending arrow icon in given header in Messages
     * section
     *
     * @param column -the header column in messages section when icon is
     *        expected
     * @return true if displayed, false otherwise
     */
    public boolean isMessagesDescendingArrowDisplayed(MessageColumns column) {
        return isElementDisplayed(getMessageColumnDescendingArrowIcon(column));
    }

    /**
     * Checks to see if Invalid Non-Numeric Steps error icon/tooltip is
     * displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isInvalidNonNumericStepsValueErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(INVALID_NON_NUMERIC_STEPS_VALUE_ERROR_TEXT));
    }

    /**
     * Checks to see if Steps exceeding maximum error icon/tooltip is displayed
     *
     * @return true if the icon/tooltip is displayed, false if it is not or
     *         cannot be found
     */
    public boolean isStepsExceedingMaxErrorDisplayed() {
        return isElementDisplayed(getSpecificErrorToolTip(STEPS_EXCEEDING_MAX_ERROR_TEXT));
    }

    /**
     * Checks if sorting arrow icon is displayed for the given Messages column
     * header cell
     *
     * @param name -the title of the cell where the icon is expected
     * @return true if icon was found, false if not
     */
    public boolean isMessagesArrowIconDisplayed(ColumnName name) {
        return isElementDisplayed(getMessagesColumnHeaderSortingArrowIcon(name));
    }

    ///////////
    // Interaction
    ///////////

    /**
     * Clicks the Vehicles tab and waits for alert loading message to be
     * invisible
     *
     * @return the Vehicles Partial Page
     */
    public VehiclesPartialPage clickVehiclesTab() {
        getDetailsTab(ExecutionDetailsTabs.VEHICLES_TAB).click();
        waitUntilAlertIsNotPresent(By.xpath(LOADING_TAB_ALERT_XPATH), 10);
        return getPage(VehiclesPartialPage.class);
    }

    /**
     * Clicks the Signals tab and waits for alert loading message to be
     * invisible
     *
     * @return the Signals Partial Page
     */
    public SignalsPartialPage clickSignalsTab() {
        getDetailsTab(ExecutionDetailsTabs.SIGNALS_TAB).click();
        waitUntilAlertIsNotPresent(By.xpath(LOADING_TAB_ALERT_XPATH), 10);
        return getPage(SignalsPartialPage.class);
    }

    /**
     * Clicks the Detectors tab and waits for alert loading message to be
     * invisible
     *
     * @return the Detectors Partial Page
     */
    public DetectorsPartialPage clickDetectorsTab() {
        getDetailsTab(ExecutionDetailsTabs.DETECTORS_TAB).click();
        waitUntilAlertIsNotPresent(By.xpath(LOADING_TAB_ALERT_XPATH), 10);
        return getPage(DetectorsPartialPage.class);
    }

    /**
     * Clicks Command Queue help icon
     */
    public CommandQueueOptionsHelpWindow clickCommandQueueHelp() {
        getHelpIcon(getSectionHeader(SectionHeaders.COMMAND_QUEUE)).click();
        return getPage(CommandQueueOptionsHelpWindow.class);
    }

    /**
     * Clicks Command Queue minimize icon and verifies after clicked the section
     * is collapsed and the maximize icon displays
     */
    public void clickCommandQueueMinimize() {
        getMinimizeIcon(getSectionHeader(SectionHeaders.COMMAND_QUEUE)).click();
        Assert.assertTrue("Command Queue section is not collapsed after clicking minimize.", isSectionHeaderCollapsed(SectionHeaders.COMMAND_QUEUE));
        Assert.assertTrue("Command Queue maximize icon is not visible after clicking minimize.", isMaximizeIconDisplayed(SectionHeaders.COMMAND_QUEUE));
    }

    /**
     * Clicks Command Queue maximize icon and verifies after clicked the section
     * is expanded and the minimize icon displays
     */
    public void clickCommandQueueMaximize() {
        getMaximizeIcon(getSectionHeader(SectionHeaders.COMMAND_QUEUE)).click();
        Assert.assertFalse("Command Queue section is not expanded after clicking maximize.", isSectionHeaderCollapsed(SectionHeaders.COMMAND_QUEUE));
        Assert.assertTrue("Command Queue minimize icon is not visible after clicking maxmiize.", isMinimizeIconDisplayed(SectionHeaders.COMMAND_QUEUE));
    }

    /**
     * Clicks Messages help icon
     */
    public MessagesHelpWindow clickMessagesHelp() {
        getHelpIcon(getSectionHeader(SectionHeaders.MESSAGES)).click();
        return getPage(MessagesHelpWindow.class);
    }

    /**
     * Clicks Messages minimize icon and verifies after clicked the section is
     * collapsed and the maximize icon displays
     */
    public void clickMessagesMinimize() {
        getMinimizeIcon(getSectionHeader(SectionHeaders.MESSAGES)).click();
        Assert.assertTrue("Messages section is not collapsed after clicking minimize.", isSectionHeaderCollapsed(SectionHeaders.MESSAGES));
        Assert.assertTrue("Messages maximize icon is not visible after clicking minimize.", isMaximizeIconDisplayed(SectionHeaders.MESSAGES));
    }

    /**
     * Clicks Messages maximize icon and verifies after clicked the section is
     * expanded and the minimize icon displays
     */
    public void clickMessagesMaximize() {
        getMaximizeIcon(getSectionHeader(SectionHeaders.MESSAGES)).click();
        Assert.assertFalse("Messages section is not expanded after clicking maximize.", isSectionHeaderCollapsed(SectionHeaders.MESSAGES));
        Assert.assertTrue("Messages minimize icon is not visible after clicking maxmiize.", isMinimizeIconDisplayed(SectionHeaders.MESSAGES));
    }

    /**
     * Hovers over Message column header
     *
     * @param columnHeader -the column header to hover over
     */
    public void hoverOverMessageColumnHeader(MessageColumns columnHeader) {
        getMessagesColumn(columnHeader).hover();
    }

    /**
     * Clicks Message column header (note: this clicks the header -- NOT the
     * dropdown selector)
     *
     * @param columnHeader -the column header to click
     */
    public void clickMessageColumnHeader(MessageColumns columnHeader) {
        getMessagesColumn(columnHeader).click();
    }

    /**
     * Clicks Message column header
     *
     * @param columnHeader -the column header to click
     * @return Filter Options Menu partial page class
     */
    public FilterOptionsMenu clickMessageColumnHeaderDropdown(MessageColumns columnHeader) {
        getMessagesColumnDropdown(columnHeader).click();
        return getPage(FilterOptionsMenu.class);
    }

    /**
     * Clicks Devices help icon
     */
    public DevicesHelpWindow clickDevicesHelp() {
        getHelpIcon(getSectionHeader(SectionHeaders.DEVICES)).click();
        return getPage(DevicesHelpWindow.class);
    }

    /**
     * Clicks Devices minimize icon and verifies after clicked the section is
     * collapsed and the maximize icon displays
     */
    public void clickDevicesMinimize() {
        getMinimizeIcon(getSectionHeader(SectionHeaders.DEVICES)).click();
        Assert.assertTrue("Devices section is not collapsed after clicking minimize.", isSectionHeaderCollapsed(SectionHeaders.DEVICES));
        Assert.assertTrue("Devices maximize icon is not visible after clicking minimize.", isMaximizeIconDisplayed(SectionHeaders.DEVICES));
    }

    /**
     * Clicks Devices maximize icon and verifies after clicked the section is
     * expanded and the minimize icon displays
     */
    public void clickDevicesMaximize() {
        getMaximizeIcon(getSectionHeader(SectionHeaders.DEVICES)).click();
        Assert.assertFalse("Devices section is not expanded after clicking maximize.", isSectionHeaderCollapsed(SectionHeaders.DEVICES));
        Assert.assertTrue("Devices minimize icon is not visible after clicking maxmiize.", isMinimizeIconDisplayed(SectionHeaders.DEVICES));
    }

    /**
     * Clicks the Commands button
     */
    public void clickCommandsBtn() {
        getCommandsBtn().click();
    }

    /**
     * Clicks the Inject Vehicle button
     */
    public InjectVehicleForm clickInjectVehicle() {
        getInjectVehicleBtn().click();
        return getPage(InjectVehicleForm.class);
    }

    /**
     * Clicks Speed Change
     *
     * @return the new loaded SpeedChangeForm
     */
    public SpeedChangeForm clickSpeedChange() {
        getCommandOption(CommandOptions.SPEED_CHANGE).click();
        return getPage(SpeedChangeForm.class);
    }

    /**
     * Clicks Lane Change
     *
     * @return the new loaded LaneChangeForm
     */
    public LaneChangeForm clickLaneChange() {
        getCommandOption(CommandOptions.LANE_CHANGE).click();
        return getPage(LaneChangeForm.class);
    }

    /**
     * Clicks Signal Change
     *
     * @return the new loaded SignalChangeForm
     */
    public SignalChangeForm clickSignalChange() {
        getCommandOption(CommandOptions.SIGNAL_CHANGE).click();
        return getPage(SignalChangeForm.class);
    }

    /**
     * Copies the displayed sim name value and pastes into Steps text box
     */
    public void copyPasteAlphabeticValueInStepsTextBox() {
        selectCopyAndPaste(getDisplayedSimName(), getStepsTextBox());
    }

    /**
     * Hovers over the given column header cell and verifies sorting arrow icon
     * displays
     *
     * @param name -the title of the column header cell
     */
    public void hoverOverMessagesColumn(ColumnName name) {
        getMessagesColumnTitleCell(name).hover();
        Assert.assertTrue("Sorting arrow icon does not display when the " + name.getLabel() + " cell is hovered over.", isMessagesArrowIconDisplayed(name));
    }

    ///////////
    //Utilities
    ///////////

    /**
     * Checks for all execution control buttons (Next Step, Finish, and View
     * Animation)
     */
    public void checkAllExecutionControlBtns() {
        Assert.assertTrue("Next Steps button could not be found or is not visible.", isExeuctionControlBtnDisplayed(ExecutionControlBtns.NEXT_STEP_BTN));
        Assert.assertTrue("Finish button could not be found or is not visible.", isExeuctionControlBtnDisplayed(ExecutionControlBtns.FINISH_BTN));
        Assert.assertTrue("View Animation button could not be found or is not visible.", isExeuctionControlBtnDisplayed(ExecutionControlBtns.VIEW_ANIMATION_BTN));
    }

    /**
     * Clicks the next step button when error is expected (does not wait for
     * loading icon), waits for error icon to display
     */
    public void clickNextStepErrorExpected() {
        getExecutionControlBtn(ExecutionControlBtns.NEXT_STEP_BTN).click();
        waitForStepsErrorToDisplay();
    }

    /**
     * Clicks the next step button, waits for loading alert to display and
     * disappear
     */
    public void clickNextStep() {
        getExecutionControlBtn(ExecutionControlBtns.NEXT_STEP_BTN).click();
        ETexasCommonUtils.sleep(1000); //ZZZ - waits for alert to display, if alert is going to be present (doesn't always display)
        if (isElementDisplayed(NEXT_STEP_LOADING_ALERT_BY)) {
            waitUntilAlertIsNotPresent(NEXT_STEP_LOADING_ALERT_BY, PAGE_LOAD_TIMEOUT);
        }
    }

    /**
     * Clicks the finish button and waits for loading icon to become visible and
     * then invisible
     *
     * @return the CompletedExecutionPage
     */
    public CompletedExecutionPage clickFinish() {
        getExecutionControlBtn(ExecutionControlBtns.FINISH_BTN).click();
        waitUntilAlertIsPresent(NEXT_STEP_LOADING_ALERT_BY, PAGE_LOAD_TIMEOUT);
        waitUntilAlertIsNotPresent(NEXT_STEP_LOADING_ALERT_BY, 360);
        return getPage(CompletedExecutionPage.class);
    }

    /**
     * Enters given text in the Steps text box
     *
     * @param text -the text to enter
     */
    public void enterSteps(String text) {
        getStepsTextBox().setText(text);
    }

    /**
     * Checks if the value displayed in the sim time area matches the given
     * value
     *
     * @param timeExpected -the value expected
     */
    public void checkSimTime(String timeExpected) {
        String displayedTime = getSimTime();
        Assert.assertEquals("The value displayed in the Sim Time area does not match the value expected.", timeExpected, displayedTime);
    }

    /**
     * Checks if the value displayed in the remaining steps area matches the
     * given value
     *
     * @param stepsExpected -the value expected
     */
    public void checkRemainingSteps(String stepsExpected) {
        String displayedSteps = getRemainingSteps();
        Assert.assertEquals("The value displayed in the Remaining Steps area does not match the value expected.", stepsExpected, displayedSteps);
    }

    /**
     * Checks for presence of all tabs (Vehicles, Signals, Detectors, Lane
     * Geometry, Logs, and Command History
     */
    public void checkAllActiveExecTabsDisplayed() {
        Assert.assertTrue("Vehicles tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.VEHICLES_TAB));
        Assert.assertTrue("Signals tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.SIGNALS_TAB));
        Assert.assertTrue("Detectors tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.DETECTORS_TAB));
        Assert.assertTrue("Lane Geometry tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.LANE_GEOMETRY_TAB));
        Assert.assertTrue("Logs tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.LOGS_TAB));
        Assert.assertTrue("Command History tab could not be found or is not visible.", isTabDisplayed(ExecutionDetailsTabs.COMMAND_HISTORY_TAB));
    }

    /**
     * Checks for presence of Commands and Inject Vehicle buttons
     */
    public void checkCommandQueueBtns() {
        Assert.assertTrue("Commands button could not be found or is not visible.", isCommandsBtnDisplayed());
        Assert.assertTrue("Inject Vehicle button could not be found or is not visible.", isInjectVehicleBtnDisplayed());
    }

    /**
     * Checks for presence of all Command options (Speed Change, Lane Change,
     * Signal Change)
     */
    public void checkAllCommandOptions() {
        Assert.assertTrue("Speed Change command option could not be found or is not visible.", isCommandOptionDisplayed(CommandOptions.SPEED_CHANGE));
        Assert.assertTrue("Lane Change command option could not be found or is not visible.", isCommandOptionDisplayed(CommandOptions.LANE_CHANGE));
        Assert.assertTrue("Signal Change command option could not be found or is not visible.", isCommandOptionDisplayed(CommandOptions.SIGNAL_CHANGE));
    }

    /**
     * Checks for presence of all column headers in Messages section (ID and
     * Type)
     */
    public void checkAllMessageColumnHeaders() {
        Assert.assertTrue("ID column header could not be found or is not visible in Messages section.", isMessagesColumnHeaderDisplayed(MessageColumns.ID));
        Assert.assertTrue("Type column header could not be found or is not visible in Messages section.", isMessagesColumnHeaderDisplayed(MessageColumns.TYPE));
    }

    /**
     * Checks that all displayed BSMV messages have info icons
     */
    public void checkBSMVMessageInfoIcons() {
        List<El> messages = getBSMVMessageRows();
        for (El message : messages) {
            El icon = message.el(By.xpath(INFO_ICON_XPATH_SUFFIX));
            Assert.assertTrue("Icon could not be found with BSMV message.", icon.isDisplayed());
        }
    }

    /**
     * Checks the the tool tip text displays as expected for each info icon
     * associated with the displayed BSMY messages
     */
    public void checkBSMVMessageIconToolTips() {
        List<El> messages = getBSMVMessageRows();
        for (El message : messages) {
            El icon = message.el(By.xpath(INFO_ICON_XPATH_SUFFIX));
            String toolTip = icon.getAttribute("data-qtip");
            Assert.assertEquals("Icon tool tip text not displayed as expected with BSMV message.", MESSAGES_INFO_ICON_TOOL_TIP_TEXT, toolTip);
        }
    }

    /**
     * Clicks the info icon in for the given listed BSMV message
     *
     * @param id -string value of the row id
     * @return the ViewingMessageWindow
     */
    public ViewingMessageWindow clickInfoIcon(String id) {
        getSpecificMessageInfoIcon(id).click();
        return getPage(ViewingMessageWindow.class);
    }

    /**
     * Checks for presence of all column headers in Devices section (ID, Name,
     * and Percentage)
     */
    public void checkAllDevicesColumns() {
        Assert.assertTrue("ID column header could not be found or is not visible in Devices section.", isDevicesColumnHeaderDisplayed(DevicesColumns.ID));
        Assert.assertTrue("Name column header could not be found or is not visible in Devices section.", isDevicesColumnHeaderDisplayed(DevicesColumns.ID));
        Assert.assertTrue("Percentage column header could not be found or is not visible in Devices section.", isDevicesColumnHeaderDisplayed(DevicesColumns.PERCENTAGE));
    }

    /**
     * Checks that all displayed devices have info icons
     */
    public void checkDeviceInfoIcons() {
        List<El> devices = getDeviceRows();
        for (El device : devices) {
            El icon = device.el(By.xpath(INFO_ICON_XPATH_SUFFIX));
            Assert.assertTrue("Icon could not be found with BSMV message.", icon.isDisplayed());
        }
    }

    /**
     * Checks the the tool tip text displays as expected for each info icon
     * associated with devices
     */
    public void checkDeviceIconToolTips() {
        List<El> devices = getDeviceRows();
        for (El device : devices) {
            El icon = device.el(By.xpath(INFO_ICON_XPATH_SUFFIX));
            String toolTip = icon.getAttribute("data-qtip");
            Assert.assertEquals("Icon tool tip text not displayed as expected with listed device.", DEVICES_INFO_ICON_TOOL_TIP_TEXT, toolTip);
        }
    }

    /**
     * Clicks the info icon in for the given listed device
     *
     * @param deviceName -the name of the device
     * @return the ViewDeviceWindow
     */
    public ViewDeviceWindow clickDeviceInfoIcon(String deviceName) {
        getDeviceInfoIcon(deviceName).click();
        return getPage(ViewDeviceWindow.class);
    }

    /**
     * Checks that first row contains the expected id
     *
     * @param id -the id expected in first row
     */
    public void checkFirstMessageRowId(String id) {
        El row = getMessageRows().get(0);
        String displayedId = getMessageIDCell(row).getText();
        Assert.assertEquals("First row in Messages table does not contain the expected ID", id, displayedId);
    }

    /**
     * Verifies Field Required error icon/tooltip displayed with Steps text box
     */
    public void checkStepsFieldRequiredErrorDisplayed() {
        Assert.assertTrue("Field required error not displayed as expected with Steps text box.", isFieldRequiredErrorDisplayed(STEPS_FIELD_DISPLAYED_NAME));
    }

    /////////
    // Waits
    /////////

    /**
     * {@inheritDoc}
     */
    @Override
    public void waitUntilLoaded() {
        waitForElementToBeVisible(By.xpath(SIM_TIME_AREA_XPATH));
    }

    /**
     * Waits for error associated with Steps text box to display
     */
    public void waitForStepsErrorToDisplay() {
        waitForElementToBeVisible(By.xpath(GENERIC_ERROR_TOOLTIP_XPATH_PREFIX + STEPS_FIELD_DISPLAYED_NAME + FIELD_ERROR_XPATH_SUFFIX));
    }
}
