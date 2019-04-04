package com.harmonia.qa.ETEXASWebQATests.utilities;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import com.harmonia.qa.ETEXASWebQATests.entities.CellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.CompositeSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.Detector;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasBaseEntity;
import com.harmonia.qa.ETEXASWebQATests.entities.ETexasUser;
import com.harmonia.qa.ETEXASWebQATests.entities.EmbeddedApp;
import com.harmonia.qa.ETEXASWebQATests.entities.FixedCellularDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.Lane;
import com.harmonia.qa.ETEXASWebQATests.entities.OBUDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.RSEDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.ReportDevice;
import com.harmonia.qa.ETEXASWebQATests.entities.TemplateSimulation;
import com.harmonia.qa.ETEXASWebQATests.entities.UserJarApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserNativeApp;
import com.harmonia.qa.ETEXASWebQATests.entities.UserRemoteApp;
import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.ETEXASWebQATests.enums.LaneID;
import com.harmonia.qa.ETEXASWebQATests.webdriver.bases.ETexasTestBase;

/**
 * Manages global lists of entities
 *
 * @author llaroussini
 */
public class ETexasEntityManager implements ETexasProperties {

    /**
     * The managed list of entities
     */
    private static List<ETexasBaseEntity> entities;

    /** The Entity Manager Instance */
    private static ETexasEntityManager instance;

    /** Prevent manual construction */
    private ETexasEntityManager() {
    }

    /**
     * Gets the current list of entities. If the list is null, the entity list
     * will be deserialized and returned.
     *
     * @return The current list of all entities
     */
    public static List<ETexasBaseEntity> getEntities() {
        if (entities == null) {
            entities = new ArrayList<ETexasBaseEntity>(20);
            List<ETexasBaseEntity> baseEntities = new ArrayList<ETexasBaseEntity>(20);
            try {
                baseEntities = ETexasFileUtils.readFromJsonFile(ETexasTestBase.getJsonFileName());
            }
            catch (IOException e) {
                e.printStackTrace();
                throw new Error("Error encountered in getting entities from JSON file");
            }
            if (baseEntities != null) {
                for (ETexasBaseEntity baseEntity : baseEntities) {
                    if (baseEntity.getETexasEntityType() != null) {
                        entities.add(baseEntity);
                    }
                }
            }
        }
        return entities;

    }

    /**
     * Gets an instance of Entity Manager
     *
     * @return an instance of this manager
     */
    public static ETexasEntityManager getInstance() {
        if (instance == null) {
            instance = new ETexasEntityManager();
        }
        return instance;
    }

    /**
     * Loops over all entities to find a Simulation with a given name
     *
     * @param simName the name of the simulation being searched for
     * @return the simulation with the given name, or null if not found
     */
    public static TemplateSimulation getTemplateSimulation(String simName) {
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity != null && entity.getETexasEntityType().equals(ETexasEntityType.TEMPLATE_SIMULATION)) {
                TemplateSimulation sim = (TemplateSimulation)entity;
                if (simName.equals(sim.getName())) {
                    return sim;
                }
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find report device with a given name
     *
     * @param reportDeviceName the name of the report device being searched for
     * @return the report device with the given name, or null if not found
     */
    public static ReportDevice getReportDevice(String reportDeviceName) {
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity != null && entity.getETexasEntityType().equals(ETexasEntityType.REPORT_DEVICE)) {
                ReportDevice device = (ReportDevice)entity;
                if (reportDeviceName.equals(device.getName())) {
                    return device;
                }
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find a the Admin User
     *
     * @return the admin user, or null if not found
     */
    public static ETexasUser getAdminUser() {
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity != null && entity.getETexasEntityType().equals(ETexasEntityType.USER)) {
                ETexasUser user = (ETexasUser)entity;
                if (user.getUsername().equals(defaultAdminUsername)) {
                    return user;
                }
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find an App with a given name
     *
     * @param appName the name of the app being searched for
     * @return the app with the given name, or null if not found
     */
    public static EmbeddedApp getApp(String appName) {
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity != null && entity.getETexasEntityType().equals(ETexasEntityType.BUILT_IN_APP)) {
                EmbeddedApp app = (EmbeddedApp)entity;
                if (appName.equals(app.getName())) {
                    return app;
                }
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find an Native App with a given name
     *
     * @param appName the name of the native app being searched for
     * @return the native app with the given name, or null if not found
     */
    public static UserNativeApp getNativeApp(String appName) {
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity != null && entity.getETexasEntityType().equals(ETexasEntityType.USER_NATIVE_APP)) {
                UserNativeApp app = (UserNativeApp)entity;
                if (appName.equals(app.getName())) {
                    return app;
                }
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find an Remote App with a given name
     *
     * @param appName the name of the remote app being searched for
     * @return the remote app with the given name, or null if not found
     */
    public static UserRemoteApp getRemoteApp(String appName) {
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity != null && entity.getETexasEntityType().equals(ETexasEntityType.USER_REMOTE_APP)) {
                UserRemoteApp app = (UserRemoteApp)entity;
                if (appName.equals(app.getName())) {
                    return app;
                }
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find a JAR App with a given name
     *
     * @param appName the name of the JAR app being searched for
     * @return the JAR app with the given name, or null if not found
     */
    public static UserJarApp getJARApp(String appName) {
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity != null && entity.getETexasEntityType().equals(ETexasEntityType.USER_JAR_APP)) {
                UserJarApp app = (UserJarApp)entity;
                if (appName.equals(app.getName())) {
                    return app;
                }
            }
        }
        return null;
    }

    /**
     * Gets all built in apps currently saves in entity manager
     *
     * @return list of embedded apps
     */
    public static List<EmbeddedApp> getAllEmbeddedApps() {
        List<EmbeddedApp> apps = new ArrayList<EmbeddedApp>(0);
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity.getETexasEntityType().equals(ETexasEntityType.BUILT_IN_APP)) {
                EmbeddedApp app = (EmbeddedApp)entity;
                apps.add(app);
            }
        }
        return apps;
    }

    /**
     * Loops over all entities to find a Cellular Device with a given name
     *
     * @param deviceName -the name of the device you are searching for
     * @return the device with the given name, or null if not found
     */
    public static CellularDevice getCellularDevice(String deviceName) {
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity.getETexasEntityType().equals(ETexasEntityType.CELLULAR_DEVICE)) {
                CellularDevice device = (CellularDevice)entity;
                if (device != null) {
                    if (deviceName.equals(device.getName())) {
                        return device;
                    }
                }
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find a Fixed Cellular Device with a given name
     *
     * @param deviceName -the name of the device you are searching for
     * @return the device with the given name, or null if not found
     */
    public static FixedCellularDevice getFixedCellularDevice(String deviceName) {
        for (ETexasBaseEntity entity : getTypedEntities(FixedCellularDevice.class)) {
            FixedCellularDevice device = (FixedCellularDevice)entity;
            if (entity != null) {
                if (deviceName.equals(deviceName)) {
                    return device;
                }
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find a RSE Device with a given name
     *
     * @param deviceName -the name of the device you are searching for
     * @return the device with the given name, or null if not found
     */
    public static RSEDevice getRSEDevice(String deviceName) {
        for (RSEDevice device : getTypedEntities(RSEDevice.class)) {
            if (deviceName.equals(device.getName())) {
                return device;
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find an OBU Device with a given name
     *
     * @param deviceName -the name of the device you are searching for
     * @return the device with the given name, or null if not found
     */
    public static OBUDevice getOBUDevice(String deviceName) {
        for (OBUDevice device : getTypedEntities(OBUDevice.class)) {
            if (deviceName.equals(device.getName())) {
                return device;
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find a Composite with a given name
     *
     * @param compositeName -the name of the composite you are searching for
     * @return the composite with the given name, or null if not found
     */
    public static CompositeSimulation getComposite(String compositeName) {
        for (CompositeSimulation composite : getTypedEntities(CompositeSimulation.class)) {
            if (compositeName.equals(composite.getName())) {
                return composite;
            }
        }
        return null;
    }

    /**
     * Loops over all entities to find a Lane with a given id
     *
     * @param id the id of the lane being searched for
     * @return the lane with the given id, or null if not found
     */
    public static Lane getLane(LaneID id) {
        String idStr = id.getLabel();
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity != null && entity.getETexasEntityType().equals(ETexasEntityType.LANE)) {
                Lane lane = (Lane)entity;
                if (lane.getLaneID().getLabel().contentEquals(idStr)) {
                    return lane;
                }
            }
        }

        return null;
    }

    /**
     * Generic method for getting an entity of any type by comparing its ID
     *
     * @param <T> Type of entity to be returned
     * @param id the ID of the entity which is being searched
     * @param kls the class of the entity to be returned
     * @return the entity of the given class with the specified ID, or Null if
     *         it cannot be found
     */
    public static <T extends ETexasBaseEntity> T getEntity(UUID id, Class<T> kls) {
        if (id == null || kls == null) {
            return null;
        }
        for (ETexasBaseEntity entity : getEntities()) {
            if (entity == null) {
                continue; //in case a null object was mistakenly added
            }
            boolean instanceMatch = kls.isInstance(entity);
            boolean idMatch = entity.getUuid().compareTo(id) == 0;
            if (instanceMatch && idMatch) {
                return kls.cast(entity);
            }
            boolean classMatch = entity.getClass().equals(kls);
            if (classMatch && idMatch) {
                return kls.cast(entity);
            }
        }
        return null;
    }

    /**
     * Generic method for getting an entity of a given type
     *
     * @param <T> Type of entity to be returned
     * @param kls the class of the entity to be returned
     * @return the entity of the given class, or Null if an instance of that
     *         class cannot be found
     */
    public static <T extends ETexasBaseEntity> T getEntity(Class<T> kls) {
        for (ETexasBaseEntity entity : getEntities()) {
            if ((kls.isInstance(entity) || entity.getClass().equals(kls))) {
                return kls.cast(entity);
            }
        }
        return null;
    }

    /**
     * Gets an entity from the managed list of a specified type which has a UUID
     * which DOES NOT match the passed ID.
     *
     * @param <T> Type of entity to be returned
     * @param kls the class of entity to be returned
     * @param ids a list of IDs to exclude from possible return results (e.g.
     *        get a Merchant with a UUID other than X)
     * @return an entity of the specified type with a UUID other than the passed
     *         value, or null if one cannot be found
     */
    public static <T extends ETexasBaseEntity> T getEntity(Class<T> kls, UUID... ids) {
        for (ETexasBaseEntity entity : getEntities()) {
            if (kls.isInstance(entity) || entity.getClass().equals(kls)) {
                for (int i = 0; i < ids.length; i++) {
                    UUID id = ids[i];
                    if (entity.getUuid().equals(id)) {//This indicates that the current ID matches the UUID of the entity, break and go to the next entity
                        break;
                    }
                    else if (i == ids.length - 1) { //This indicates that the ID of the current entity did not match any of the passed ids, this result is successful
                        return kls.cast(entity);
                    }
                }
            }
        }
        return null;
    }

    /**
     * Adds an entity to the managed list of entities
     *
     * @param entity the entity to be added
     */
    public static void addEntity(ETexasBaseEntity entity) {
        if (entity != null) {
            getEntities().add(entity);
        }
        else {
            for (StackTraceElement e : Thread.currentThread().getStackTrace()) {
                System.out.println(e.toString());
            }
            System.out.println();
        }
    }

    /**
     * Adds a set of entities to the managed list of entities
     *
     * @param newEntities a list of the entities to be added
     */
    public static void addEntities(List<ETexasBaseEntity> newEntities) {
        for (ETexasBaseEntity entity : newEntities) {
            addEntity(entity);
        }
    }

    /**
     * Adds a set of entities to the managed list of entities
     *
     * @param newEntities a list of entities to be added
     */
    public static void addEntities(ETexasBaseEntity... newEntities) {
        for (ETexasBaseEntity entity : newEntities) {
            addEntity(entity);
        }
    }

    /**
     * Retrieves a list of all entities of the specified type from the managed
     * list. The returned list will include any instances (e.g. child classes)
     * as well
     *
     * @param <T> The type of entity being fetched
     * @param kls the class of entity being fetched
     * @return a list of entities of the specified type
     */
    public static <T extends ETexasBaseEntity> List<T> getTypedEntities(Class<T> kls) {
        List<T> typedEntities = new ArrayList<T>(1);
        for (ETexasBaseEntity entity : getEntities()) {
            if ((kls.isInstance(entity) || entity.getClass().equals(kls))) {
                typedEntities.add(kls.cast(entity));
            }
        }
        return typedEntities;

    }

    /**
     * Loops over all entities to find a Detector with a given height
     *
     * @param height the height of the detector that you're searching for
     * @return the detector with the given height, or null if not found
     */
    public static Detector getDetector(String height) {
        for (Detector detector : getTypedEntities(Detector.class)) {
            if (height.equals(detector.getHeight())) {
                return detector;
            }
        }
        return null;
    }
}
