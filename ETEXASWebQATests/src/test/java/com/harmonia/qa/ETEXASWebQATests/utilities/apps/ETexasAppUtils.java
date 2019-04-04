package com.harmonia.qa.ETEXASWebQATests.utilities.apps;

import java.io.File;
import java.io.IOException;

import org.junit.Assert;

import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.utilities.ETexasCommonUtils;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.AppsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.LandingPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.SimulationsPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.CreateJARApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.CreateNativeApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.CreateRemoteApplicationModal;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.JARApplicationsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.NativeAppsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.NativeAppsPartialPage.NativeAppColumn;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.RemoteApplicationsPartialPage;
import com.harmonia.qa.ETEXASWebQATests.webdriver.pages.partial.apps.RemoteApplicationsPartialPage.RemoteAppColumn;

/**
 * Utility class to assist with Apps
 *
 * @author llaroussini
 */
public class ETexasAppUtils extends ETexasCommonUtils {

    /**
     * Creates a Native App in the UI. Assumes the user is not logged in, and
     * includes navigating to the landing page. Also assumes that the app being
     * passed has all values (i.e. the user has been set) are correctly set and
     * exist in the application. This method DOES verify the app is displayed
     * following creation and then logs user out of the application
     *
     * @param app the native app to create
     */
    public static void createNativeApp(UserNativeApp app) {
        ETexasUser user = app.getUser();
        LandingPage landingPage = goToLandingPage();
        landingPage.waitUntilLoaded();
        SimulationsPage simPage = landingPage.loginAs(user);
        simPage.waitUntilLoaded();
        AppsPage appsPage = simPage.clickApps();
        NativeAppsPartialPage nativeTab = appsPage.clickNativeAppsTab();
        CreateNativeApplicationModal createModal = nativeTab.clickCreate();
        createModal.setAllFields(app);
        createModal.clickCreate();
        Assert.assertTrue("Native app with name: " + app.getName() + " is not displayed after being created.", nativeTab.isAppDisplayed(app.getName()));
        app.setID(nativeTab.getNativeAppCellValue(app, NativeAppColumn.ID));
        simPage.logout(user);
    }

    /**
     * Creates a Remote App in the UI. Assumes the user is not logged in, and
     * includes navigating to the landing page. Also assumes that the app being
     * passed has all values (i.e. the user has been set) are correctly set and
     * exist in the application. This method DOES verify the app is displayed
     * following creation and then logs user out of the application
     *
     * @param app the remote app to create
     */
    public static void createRemoteApp(UserRemoteApp app) {
        ETexasUser user = app.getUser();
        LandingPage landingPage = goToLandingPage();
        landingPage.waitUntilLoaded();
        SimulationsPage simPage = landingPage.loginAs(user);
        simPage.waitUntilLoaded();
        AppsPage appsPage = simPage.clickApps();
        RemoteApplicationsPartialPage remoteAppsPage = appsPage.clickRemoteAppsTab();
        CreateRemoteApplicationModal createModal = remoteAppsPage.clickCreate();
        createModal.setAllFields(app);
        createModal.clickCreate(true);
        Assert.assertTrue("Remote app with name: " + app.getName() + " is not displayed after being created.", remoteAppsPage.isAppDisplayed(app));
        app.setID(remoteAppsPage.getRemoteAppCellValue(app, RemoteAppColumn.ID));
        simPage.logout(user);
    }

    /**
     * Creates a JAR App in the UI. Assumes the user is not logged in, and
     * includes navigating to the landing page. Also assumes that the app being
     * passed has all values (i.e. the user has been set) are correctly set and
     * exist in the application. This method DOES verify the app is displayed
     * following creation and then logs user out of the application
     *
     * @param app the JAR app to create
     * @param jarFile the JAR file being uploaded
     * @throws IOException if errors are encountered reading the file name/path
     *         - likely an issue with file permissions
     */
    public static void createJARApp(UserJarApp app, File jarFile) throws IOException {
        ETexasUser user = app.getUser();
        LandingPage landingPage = goToLandingPage();
        SimulationsPage simPage = landingPage.loginAs(user);
        AppsPage appsPage = simPage.clickApps();
        JARApplicationsPartialPage jarAppsPage = appsPage.clickJARAppsTab();
        CreateJARApplicationModal createModal = jarAppsPage.clickCreate();
        createModal.setJARName(app);
        createModal.setUploadFile(jarFile);
        createModal.clickCreateBtnAndWait();
        Assert.assertTrue("JAR of applications with name: " + app.getName() + " is not displayed after being created.", jarAppsPage.isAppDisplayed(app.getName()));
        app.setID(jarAppsPage.getJARAppID(app.getName()));
        simPage.logout(user);
    }
}
