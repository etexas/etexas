package com.harmonia.qa.ETEXASWebQATests.entities;

import java.io.IOException;
import java.io.Serializable;
import java.util.UUID;

import com.cedarsoftware.util.io.JsonReader;
import com.harmonia.qa.ETEXASWebQATests.enums.ETexasEntityType;
import com.harmonia.qa.Entities.BaseEntity;

/**
 * Base entity class extended by all entity classes
 *
 * @author llaroussini
 */
public class ETexasBaseEntity extends BaseEntity implements Serializable {

    /**
     * Auto generated serial UID
     */
    private static final long serialVersionUID = 413060524671172148L;

    /**
     * Entity's UUID
     */
    private UUID uuid;

    /**
     * Entity's Type
     */
    protected ETexasEntityType entityType;

    /**
     * Gets the type of entity
     *
     * @return the type of the entity
     */
    public ETexasEntityType getETexasEntityType() {
        return this.entityType;
    }

    /**
     * Default constructor
     */
    public ETexasBaseEntity() {
        this.uuid = UUID.randomUUID();
        this.entityType = ETexasEntityType.BASE_ENTITY;
    }

    /**
     * Overridden comparison method.
     *
     * @return true if the passed object is determined to be a Base Entity with
     *         the same UUID as this entity. False otherwise.
     */
    @Override
    public boolean equals(Object obj) {
        if (obj == null) {
            return false;
        }
        else if (!(obj instanceof ETexasBaseEntity)) {
            return false;
        }
        else if (((ETexasBaseEntity)obj).getUuid() == null) {
            return false;
        }
        else if (((ETexasBaseEntity)obj).getUuid().compareTo(this.getUuid()) == 0) {
            return true;
        }
        else {
            return false;
        }
    }

    /**
     * <p>
     * Provides a clone of this entity which has all the same values but breaks
     * any references between the returned entity and this. The returned entity
     * will also have a DIFFERENT randomly assigned UUID.
     * </p>
     * <p>
     * The clone operation in this method is accomplished by serializing the
     * entity (using toString()) and then deserializing it.
     * </p>
     */
    @Override
    public ETexasBaseEntity clone() {
        try {
            ETexasBaseEntity entity = (ETexasBaseEntity)JsonReader.jsonToJava(this.toString());
            entity.setUuid(UUID.randomUUID());
            return entity;
        }
        catch (IOException e) {
            e.printStackTrace();
            return null;
        }

    }

}
