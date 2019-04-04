package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.compositeSettings;
//package com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.devices;
//
//import org.junit.Assert;
//import org.openqa.selenium.By;
//import org.openqa.selenium.WebDriver;
//
//import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
//import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
//import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
//import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
//import com.harmonia.qa.ETEXASWebQATests.enums.DeviceType;
//import com.harmonia.qa.webdriver.utilities.elements.El;
//TODO Update for 3.0
///**
// * Page class representing the overview of device configuration partial page
// *
// * @author llaroussini
// */
//public class OverviewDeviceConfigurationPartialPage extends CompositeSettingsModal {
//
//	/**
//	 * Default constructor
//	 *
//	 * @param driver -the web driver
//	 */
//	public OverviewDeviceConfigurationPartialPage(WebDriver driver) {
//		super(driver);
//	}
//
//	//////////////
//	//Enumerations
//	//////////////
//
//	/**
//	 * Enumeration of columns in Overview devices table
//	 *
//	 * @author llaroussini
//	 */
//	public enum OverviewDeviceTableColumnHeader {
//		/**
//		 * ID column header
//		 */
//		ID("ID"),
//		/**
//		 * Device Name column header
//		 */
//		DEVICE_NAME("Device Name"),
//		/**
//		 * X Coordinate column header
//		 */
//		DEVICE_TYPE("Device Type");
//
//		/**
//		 * The label of the column header as appears in the application
//		 */
//		private String label;
//
//		/**
//		 * Default constructor; sets the label
//		 *
//		 * @param label The string to set as the label
//		 */
//		OverviewDeviceTableColumnHeader(String label) {
//			this.label = label;
//		}
//
//		/**
//		 * Gets the label associated with the column header as it is displayed
//		 * in the Web UI
//		 *
//		 * @return The label of the column header
//		 */
//		public String getLabel() {
//			return this.label;
//		}
//	}
//
//	///////////
//	//ID's & Locators
//	///////////
//
//	/**
//	 * Xpath prefix to Overview table column
//	 */
//	private static final String OVERVIEW_DEVICES_TABLE_COLUMN_XPATH_PREFIX = "//div[contains(@id, 'simDeviceConfigDialog')]//div[contains(@id, 'devicepanel')][1]//span[contains(@id, 'gridcolumn')][text()='";
//
//	/**
//	 * Xpath prefix to configure buttons
//	 */
//	private static final String CONFIGURE_DEVICES_BTN_XPATH_PREFIX = "//div[contains(@id, 'devicepanel')]//span[contains(@id, 'button')][text()='";
//
//	/**
//	 * Gets the given column header element
//	 *
//	 * @param column -the column to get
//	 * @return the column header element
//	 */
//	private El getOverviewDeviceColumnHeader(OverviewDeviceTableColumnHeader column) {
//		return el(By.xpath(OVERVIEW_DEVICES_TABLE_COLUMN_XPATH_PREFIX + column.label + "']"));
//	}
//
//	///////////
//	// Getters
//	///////////
//
//	/**
//	 * Gets the given overview button
//	 *
//	 * @param btn -the button to get
//	 * @return the overview button
//	 */
//	private El getOverviewBtn(ConfigureBtn btn) {
//		return el(By.xpath(CONFIGURE_DEVICES_BTN_XPATH_PREFIX + btn.getLabel() + "']"));
//	}
//
//	///////////
//	//Checkers
//	///////////
//
//	/**
//	 * Checks to see if the column header expected is displayed
//	 *
//	 * @param column -the column expected
//	 * @return true if displayed, false otherwise
//	 */
//	public boolean isOverviewDeviceColumnHeaderDisplayed(OverviewDeviceTableColumnHeader column) {
//		return isElementDisplayed(getOverviewDeviceColumnHeader(column));
//	}
//
//	/**
//	 * Checks to see if the overview configure button is displayed
//	 *
//	 * @return true if the cellular configure button is displayed, false if it
//	 *         is not or cannot be found
//	 */
//	public boolean isOverviewConfigureBtnDisplayed() {
//		return isElementDisplayed(getOverviewBtn(ConfigureBtn.CONFIGURE));
//	}
//
//	/**
//	 * Checks to see if the overview delete button is displayed
//	 *
//	 * @return true if the cellular delete button is displayed, false if it is
//	 *         not or cannot be found
//	 */
//	public boolean isOverviewDeleteBtnDisplayed() {
//		return isElementDisplayed(getOverviewBtn(ConfigureBtn.DELETE));
//	}
//
//	/**
//	 * Checks for the presence of the Configure device and Delete buttons
//	 */
//	public void checkOverviewBtns() {
//		Assert.assertTrue("The Configure button was not found.", isOverviewConfigureBtnDisplayed());
//		Assert.assertTrue("The Delete button was not found.", isOverviewDeleteBtnDisplayed());
//	}
//
//	///////////
//	//Utilities
//	///////////
//
//	/**
//	 * Verifies all column headers are displayed in overview devices table (ID,
//	 * Device Name, Device Type)
//	 */
//	public void checkOverviewDeviceColumnHeaders() {
//		Assert.assertTrue("The ID column header not displayed as expected in Overview Devices table.", isOverviewDeviceColumnHeaderDisplayed(OverviewDeviceTableColumnHeader.ID));
//		Assert.assertTrue("The Device Name column header not displayed as expected in Overview Devices table.", isOverviewDeviceColumnHeaderDisplayed(OverviewDeviceTableColumnHeader.DEVICE_NAME));
//		Assert.assertTrue("The Device Type column header not displayed as expected in Overview Devices table.", isOverviewDeviceColumnHeaderDisplayed(OverviewDeviceTableColumnHeader.DEVICE_TYPE));
//	}
//
//	/**
//	 * Verifies the given device name is displayed in the overview devices table
//	 *
//	 * @param deviceName -the name of the device expected
//	 */
//	public void checkDeviceNameDisplayed(String deviceName) {
//		Assert.assertTrue("Device with name: " + deviceName + " could not be found.", isDeviceDisplayed(deviceName));
//	}
//
//	/**
//	 * Verifies the rse device is displayed in the overview devices table
//	 *
//	 * @param rse -the rse device expected
//	 */
//	public void checkDeviceNameDisplayed(RSEDevice rse) {
//		String rseName = rse.getName();
//		checkDeviceNameDisplayed(rseName);
//	}
//
//	/**
//	 * Verifies the obu device is displayed in the overview devices table
//	 *
//	 * @param obu -the obu device expected
//	 */
//	public void checkDeviceNameDisplayed(OBUDevice obu) {
//		String obuName = obu.getName();
//		checkDeviceNameDisplayed(obuName);
//	}
//
//	/**
//	 * Verifies the cellular device is displayed in the overview devices table
//	 *
//	 * @param cellular -the cellular device expected
//	 */
//	public void checkDeviceNameDisplayed(CellularDevice cellular) {
//		String cellularName = cellular.getName();
//		checkDeviceNameDisplayed(cellularName);
//	}
//
//	/**
//	 * Verifies the fixed cellular device is displayed in the overview devices
//	 * table
//	 *
//	 * @param fixedCell -the fixed cellular device expected
//	 */
//	public void checkDeviceNameDisplayed(FixedCellularDevice fixedCell) {
//		String fixedCellName = fixedCell.getName();
//		checkDeviceNameDisplayed(fixedCellName);
//	}
//
//	/**
//	 * Verifies given device name and device type are displayed (checks that
//	 * they display in the same row)
//	 *
//	 * @param deviceName -name of the device expected
//	 * @param deviceType -type of device expected
//	 */
//	public void checkDeviceDisplayed(String deviceName, DeviceType deviceType) {
//		String displayedDeviceType = deviceType.getLabel();
//		Assert.assertTrue("Device with name: " + deviceName + " could not be found.", isDeviceDisplayed(deviceName));
//		Assert.assertTrue("Expected device type of: " + displayedDeviceType + " for device with name: " + deviceName + " could not be found.", isDeviceTypeDisplayed(deviceName, displayedDeviceType));
//	}
//
//	/**
//	 * Verifies given device is displayed (checks both name and device type
//	 * displayed)
//	 *
//	 * @param rse - the rse device expected
//	 */
//	public void checkDeviceDisplayed(RSEDevice rse) {
//		String deviceName = rse.getName();
//		DeviceType deviceType = rse.getDeviceType();
//		checkDeviceDisplayed(deviceName, deviceType);
//	}
//
//	/**
//	 * Verifies given device is displayed (checks both name and device type
//	 * displayed)
//	 *
//	 * @param obu - the obu device expected
//	 */
//	public void checkDeviceDisplayed(OBUDevice obu) {
//		String deviceName = obu.getName();
//		DeviceType deviceType = obu.getDeviceType();
//		checkDeviceDisplayed(deviceName, deviceType);
//	}
//
//	/**
//	 * Verifies given device is displayed (checks both name and device type
//	 * displayed)
//	 *
//	 * @param cellular - the cellular device expected
//	 */
//	public void checkDeviceDisplayed(CellularDevice cellular) {
//		String deviceName = cellular.getName();
//		DeviceType deviceType = cellular.getDeviceType();
//		checkDeviceDisplayed(deviceName, deviceType);
//	}
//
//	/**
//	 * Verifies given device is displayed (checks both name and device type
//	 * displayed)
//	 *
//	 * @param fixed cellular - the fixed cellular device expected
//	 */
//	public void checkDeviceDisplayed(FixedCellularDevice fixedCellular) {
//		String deviceName = fixedCellular.getName();
//		DeviceType deviceType = fixedCellular.getDeviceType();
//		checkDeviceDisplayed(deviceName, deviceType);
//	}
//
//	/**
//	 * {@inheritDoc}
//	 */
//	@Override
//	public void waitUntilLoaded() {
//		waitForElementToBeVisible(By.xpath(OVERVIEW_DEVICES_TABLE_COLUMN_XPATH_PREFIX + OverviewDeviceTableColumnHeader.DEVICE_TYPE.label + "']"), 10);
//	}
//}
