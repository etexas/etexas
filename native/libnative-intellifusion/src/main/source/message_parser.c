/**
 * Defines the functions to implement algorithms that parse vehicles, lane
 * managers, and signal indications from BSM, MAP, and SPaT messages,
 * respectively.
 *
 * @author emyers
 */

// system files
#include <limits.h>  // INT_MIN
#include <math.h>    // sqrt

// library files
#include <BasicSafetyMessage.h>  // BSM data structures
#include <Intersection.h>        // intersection data structures
#include <MapData.h>             // MAP data structures
#include <SPAT.h>                // SPaT data structures

// project files
#include "intellifusion_config.h"      // BSM filter radius, lane assignment id
#include "intellifusion_data_model.h"  // model data structures
#include "message_parser.h"            // function implementations

// constants
static const double CONVERT_TO_RADIANS_COEFFICIENT =
    (3.14159265358979323846 / 180.0);

// forward type declarations
typedef struct Polygon Polygon;

/**
 * A polygon with coordinate vertices.
 */
struct Polygon {

  /**
   * The array of x coordinates for polygon vertices.
   */
  double *xs;

  /**
   * The array of y coordinates for polygon vertices.
   */
  double *ys;
};

// forward function declarations
static int AssignVehicleToLane(
    Vehicle *vehicle, LaneManager *lane_manager);
static Vehicle *BlobToVehicle(uint8_t *blob, LaneManager *lane_manager,
    VehicleList *known_vehicles);
static int BuildInt(uint8_t *buffer, int starting_index, int bytes);
static void BuildLaneMovements(long attributes, Lane *lane);
static int BuildLanes(
    Lane *lanes, LaneType lane_type, Approach_t *approach, double width);
static Polygon* BuildPolygonFromNodes(LaneNode *node1, LaneNode *node2);
static int CalcNumLanes(Intersection_t *intersection);
static int16_t ConvertToInt16(uint8_t *buffer, int index);
static double *ConvertToXy(
    double lat_ref, double lon_ref, double lat, double lon);
static int ParseLaneNum(uint8_t *blob);
static LaneNode *ParseNodes(NodeList_t *nodes, double width);
static void ParseOctetToNode(
    OCTET_STRING_t *octet, LaneNode *node, double width);
static SignalIndicationList *ParseSpatIntersection(
    IntersectionState_t *intersection, SignalIndicationList *signals);
static int ParseSpatIntersectionId(IntersectionID_t id);
static int ParseSpatIntersectionStatusObject(
    IntersectionStatusObject_t status_object);
static SignalIndicationList *ParseSpatMovementState(
    int intersection_id, int intersection_status, MovementState_t *movement,
    SignalIndicationList *signals);
static SignalIndicationList *ParseSpatSignalIndication(
    long indication, SignalIndicationList *signals, int lane_id,
    int intersection_id, double time_to_change);
static int PointInLane(double x, double y, Lane *lane);
static int PointInLaneNodes(
    double x, double y, LaneNode *node1, LaneNode *node2);
static int PointWithinRadius(double x, double y, LaneNode *node2);
static int PointInPoly(
    int vertex_count, double testx, double testy, double *vertx,
    double *verty);
static int VehicleInRange(Vehicle *vehicle);

/*
 * Sets the lane ID of the vehicle to be the ID of the lane that contains the vehicle.
 * No lane ID will be set if the vehicle does not reside in the lane.
 *
 * @param vehicle the vehicle to assign to a lane
 * @param lane_manager a pointer to the lane manager
 * @return zero if no lane was set, non-zero otherwise
 */
static int AssignVehicleToLane(
    Vehicle *vehicle, LaneManager *lane_manager) {

  int i, found;
  for (i = 0, found = 0; (i < lane_manager->lane_count) && (found == 0); i++) {
    if (PointInLane(vehicle->x, vehicle->y, &(lane_manager->lanes[i]))) {
      found = 1;
      vehicle->lane_id = lane_manager->lanes[i].id;
    }
  }
  return found;
}

/**
 * Returns a pointer to a vehicle parsed from the bytes in the given array.
 *
 * @param blob a pointer to the array of bytes containing vehicle information
 * @param lane_manager a pointer to the lane manager
 * @param known_vehicles a list of vehicles which are previously known to exist in a particular lane
 * @return a pointer to the vehicle parsed from the given array of bytes (or
 * null if a vehicle with an assigned lane cannot be parsed)
 */
static Vehicle *BlobToVehicle(uint8_t *blob, LaneManager *lane_manager,
    VehicleList *known_vehicles) {

  int length_width;
  Vehicle *vehicle = (Vehicle *) malloc(sizeof(struct Vehicle));
  double *xy;

  vehicle->id = BuildInt(blob, 1, 4);
  double latitude = (BuildInt(blob, 7, 4) * 0.0000001);
  double longitude = BuildInt(blob, 11, 4) * 0.0000001;
  //converts from decimeters to meters
  double elevation = BuildInt(blob, 15, 2) * 0.1;

  xy = ConvertToXy(
      lane_manager->latitude, lane_manager->longitude, latitude, longitude);

  vehicle->x = xy[0];
  vehicle->y = xy[1];
  vehicle->z = 0.0;

  // exclude vehicles outside the BSM radius
  if (!VehicleInRange(vehicle)) {
    free(vehicle);
    return NULL;
  }

  // use lane assignment value unless invalid, otherwise assign lanes based on position
  if (GetLaneAssignmentId() > 0) {

    vehicle->lane_id = GetLaneAssignmentId();

  } else {

    // need to figure out which lane a vehicle is in here...
    int found = AssignVehicleToLane(vehicle,
        lane_manager);

    // if the vehicle is not assigned to a lane just give it the last known lane
    if (!found) {
      Vehicle *prev_vehicle = GetVehicleById(known_vehicles, vehicle->id);
      if (prev_vehicle != NULL) {
        vehicle->lane_id = prev_vehicle->lane_id;
      } else {
        vehicle->lane_id = -1;
      }
    }
  }

  int speed = blob[21];
  speed <<= 8;
  speed |= blob[22];
  vehicle->speed = (double) (speed & 0b1111111111111);
  vehicle->speed *= 0.02;

  int heading = blob[23];
  heading <<= 8;
  heading |= blob[24];

  // The heading is in the units of 0.0125 degrees
  vehicle->heading = heading * 0.0125;

  length_width = (int) blob[35];
  length_width <<= 8;
  length_width |= (int) blob[36];
  length_width <<= 8;
  length_width |= (int) blob[37];

  vehicle->width = (double) (length_width & 0b1111111111);
  vehicle->length = (double) (length_width >> 10);

  // unparsed constants
  vehicle->type = VehicleType_car;

  return vehicle;
}

/**
 * Returns an integer parsed from the given array of bytes.
 *
 * @param buffer a pointer to the array of bytes
 * @param starting_index the starting index of the integer bytes
 * @param bytes the number of bytes to parse.
 * @return the parsed integer
 */
static int BuildInt(uint8_t *buffer, int starting_index, int bytes) {

  int integer = buffer[starting_index] & 255;

  int i;
  for (i = starting_index + 1; i < bytes + starting_index; i++) {
    integer <<= 8;
    integer |= ((int) buffer[i] & 255);
  }

  return integer;
}

/**
 * Updates the given lane with movement data extracted from the given lane
 * attributes.
 *
 * @param attributes the lane attributes
 * @param ret a pointer to the lane to be updated
 */
static void BuildLaneMovements(long attributes, Lane *lane) {

  lane->movement_count = 0;

  if (attributes & 4) {
    lane->movement_count++;
  }

  if (attributes & 8) {
    lane->movement_count++;
  }

  if (attributes & 2) {
    lane->movement_count++;
  }

  if (attributes & 64) {
    lane->movement_count++;
  }

  lane->movements = (LaneMovementType *) malloc(
      sizeof(LaneMovementType) * lane->movement_count);

  int i = 0;

  if (attributes & 4) {

    lane->movements[i] = LEFT_TURN;
    i++;
  }

  if (attributes & 8) {

    lane->movements[i] = RIGHT_TURN;
    i++;
  }

  if (attributes & 64) {

    lane->movements[i] = NO_TURN_ON_RED;
    i++;
  }

  if (attributes & 2) {

    lane->movements[i] = STRAIGHT;
  }
}

/**
 * Returns the number of lanes built by updating the given array of lanes with
 * data extracted from the given approach. The default width is used if no
 * width is found when parsing lane nodes.
 *
 * @param lanes the array of lanes to update
 * @param lane_type the type of lane (inbound or outbound)
 * @param approach the intersection approach
 * @param width the default width when no other width can be parsed
 * @return the number of lanes built from the given approach
 */
static int BuildLanes(
    Lane *lanes, LaneType lane_type, Approach_t *approach, double width) {

  if (approach->drivingLanes == NULL) {
    return 0;
  }

  int i;
  for (i = 0; i < approach->drivingLanes->list.count; i++) {
    VehicleReferenceLane_t *lane = approach->drivingLanes->list.array[i];
    lanes[i].id = ParseLaneNum(lane->laneNumber.buf);
    lanes[i].nodes = ParseNodes(&(lane->nodeList), width);
    lanes[i].node_count = lane->nodeList.list.count;
    lanes[i].type = lane_type;

    BuildLaneMovements(lane->laneAttributes, &(lanes[i]));
  }

  return approach->drivingLanes->list.count;
}

/**
 * Returns a pointer to a polygon built around the given lane nodes.
 *
 * @param node1 a pointer to the first lane node for polygon construction
 * @param node2 a pointer to the second lane node for polygon construction
 * @return a pointer to the polygon containing the given lane nodes
 */
static Polygon *BuildPolygonFromNodes(LaneNode *node1, LaneNode *node2) {

  double tx = node2->x - node1->x;
  double ty = node2->y - node1->y;

  // get the normal orthogonal vector of the path from p1 to p2
  double otx = ty;
  double oty = -tx;
  double ot_mag = sqrt(otx * otx + oty * oty);

  // find the four points of the rectangle
  double p1_half_width = node1->width / 2;
  double p2_half_width = node2->width / 2;
  Polygon *polygon = (Polygon *) malloc(sizeof(Polygon));

  polygon->xs = (double*) malloc(sizeof(double) * 4);
  polygon->ys = (double*) malloc(sizeof(double) * 4);

  otx = otx / ot_mag;
  oty = oty / ot_mag;

  polygon->xs[0] = node1->x - otx * p1_half_width;
  polygon->xs[1] = node1->x + otx * p1_half_width;
  polygon->xs[2] = node2->x + otx * p2_half_width;
  polygon->xs[3] = node2->x - otx * p2_half_width;

  polygon->ys[0] = node1->y - oty * p1_half_width;
  polygon->ys[1] = node1->y + oty * p1_half_width;
  polygon->ys[2] = node2->y + oty * p2_half_width;
  polygon->ys[3] = node2->y - oty * p2_half_width;

  return polygon;
}

/**
 * Returns the number of lanes for the given intersection. The number of lanes
 * includes both inbound and outbound lanes.
 *
 * @param intersection the intersection
 * @return the number of lanes for the given intersection
 */
static int CalcNumLanes(Intersection_t *intersection) {

  int lanes = 0;

  int i;
  for (i = 0; i < intersection->approaches.list.count; i++) {
    if (intersection->approaches.list.array[i]->approach != NULL) {
      Approach_t *approach = intersection->approaches.list.array[i]->approach;
      if (approach->drivingLanes != NULL) {
        lanes += approach->drivingLanes->list.count;
      }
    }

    if (intersection->approaches.list.array[i]->egress != NULL) {
      Approach_t* approach = intersection->approaches.list.array[i]->egress;
      if (approach->drivingLanes != NULL) {
        lanes += approach->drivingLanes->list.count;
      }
    }
  }

  return lanes;
}

/**
 * Returns a signed integer parsed from 2 bytes in the given array of bytes.
 *
 * @param buf the buffer containing the 2 bytes
 * @param index the starting index of the bytes to parse
 * @return the parsed signed integer
 */
static int16_t ConvertToInt16(uint8_t *buffer, int index) {

  int16_t x = (int16_t) buffer[index];
  x &= 255;
  x <<= 8;
  x |= (int16_t)(buffer[index + 1] & 255);

  return x;
}

/*
 * Returns a pointer to an array of doubles that includes the x and y
 * coordinates converted from the given latitude and longitude values. The
 * technique originated from
 * http://mathforum.org/library/drmath/view/51833.html.
 *
 * @param lat_ref the reference latitude (treated as 0)
 * @param lon_ref the reference longitude (treated as 0)
 * @param lat the latitude to convert
 * @param lon the longitude to convert
 * @return a pointer to an array of two doubles for the x and y conversion
 * results
 */
//TODO ttevendale - this is setting up lat, long as if the earth were a perfect ball
// Digikam::GeodeticCalculator should be researched to see if it would make more sense here(or some other calculator)
static double *ConvertToXy(
    double lat_ref, double lon_ref, double lat, double lon) {

  double *coordinates;
  double x;
  double y;

  /*
   * Constant notes: 637810000 is the radius of the earth in centimeters
   * according to google. PI is 3.14159265358979323846 according to the
   * cmath.h header. Also, cmath.h was not cooperating as a dependency.
   */
  y = 637810000 * (lat - lat_ref) * CONVERT_TO_RADIANS_COEFFICIENT;
  x = 637810000 * (lon - lon_ref) * CONVERT_TO_RADIANS_COEFFICIENT
      * cos(lat_ref);

  coordinates = (double *) malloc(sizeof(double) * 2);
  coordinates[0] = x;
  coordinates[1] = y;

  return coordinates;
}

/**
 * Returns a pointer to a vehicle parsed from the given BSM.
 *
 * @param bytes a pointer to the BSM
 * @param size the length (number of bytes) of the message
 * @param lane_manager a pointer to the lane manager
 * @param known_vehicles a list of vehicles which are previously known to exist in a particular lane
 * @return a pointer to the parsed vehicle
 */
Vehicle *ParseBsm(char *bytes, int size, LaneManager *lane_manager,
    VehicleList *known_vehicles) {

  /**
   * The following is a check to see if the bytes are a mapdata message
   * because mapdata messages can be returned as a RC_OK through the
   * asn_DEF_BasicSafetyMessage.ber_decoder which is undesired behavior
   */
  MapData_t *map_dat = 0;
  asn_dec_rval_t mapRval;
  mapRval = asn_DEF_MapData.ber_decoder(0, &asn_DEF_MapData, (void**) &map_dat,
      bytes, size, 0);
  if (mapRval.code == RC_OK) {
    return NULL;
  }

  BasicSafetyMessage_t *bsm = 0;
  asn_dec_rval_t bsmRval;
  Vehicle *vehicle;

  bsmRval = asn_DEF_BasicSafetyMessage.ber_decoder(
      0, &asn_DEF_BasicSafetyMessage, (void **) &bsm, bytes, size, 0);

  if (bsmRval.code == RC_OK) {
    vehicle = BlobToVehicle(bsm->blob1.buf, lane_manager, known_vehicles);

  } else {

    vehicle = NULL;
  }

  asn_DEF_BasicSafetyMessage.free_struct(&asn_DEF_BasicSafetyMessage, bsm, 0);

  return vehicle;
}

/**
 * Returns a lane number parsed from the given array of bytes.
 *
 * @param blob the array of bytes to parse
 * @return the parsed lane number
 */
static int ParseLaneNum(uint8_t *blob) {

  return ((int) blob[0]) & 255;
}

/**
 * Returns a pointer to a list of lane managers (one for each intersection)
 * parsed from the given map data message.
 *
 * @param bytes a pointer to the map data message
 * @param size the length (number of bytes) of the map data message
 * @return a pointer to the parsed list of lane managers
 */
LaneManagerList *ParseMapData(char *bytes, int size) {

  MapData_t *map_dat = 0;
  LaneManagerList *lane_managers = NULL;
  LaneManagerList *tmp;
  asn_dec_rval_t rval;
  LaneManager *lane_manager;

  // iterators
  int i;
  int j;
  int x;

  rval = asn_DEF_MapData.ber_decoder(0, &asn_DEF_MapData, (void**) &map_dat,
      bytes, size, 0);

  if (rval.code == RC_OK) {

    struct MapData__intersections *inter = map_dat->intersections;

    int j;
    for (j = 0; j < inter->list.count; j++) {
      double width = 0.0;

      lane_manager = (LaneManager *) malloc(sizeof(struct LaneManager));
      // this is saying only grab the first intersection
      Intersection_t *intersect = (Intersection_t *) (inter->list.array[0]);

      if (intersect->refPoint) {

        Position3D_t *r = intersect->refPoint;
        lane_manager->latitude = (r->lat) / 10000000.0;
        lane_manager->longitude = (r->Long) / 10000000.0;
        //the producer doesn't currently send the elevation
        //lane_manager->elevation = BuildInt(r->elevation, 0, 2) /10.0;
      }

      if (intersect->laneWidth != NULL) {
        width = (double) (*(intersect->laneWidth));
      }

      lane_manager->lane_count = CalcNumLanes(intersect);
      int lane_count = lane_manager->lane_count;

      Lane *lane_tmp = (Lane *) malloc(sizeof(struct Lane)
          * lane_manager->lane_count);

      i = 0;
      for (x = 0; x < intersect->approaches.list.count; x++) {

        ApproachObject_t *obj = intersect->approaches.list.array[x];

        if (obj->laneWidth != NULL) {
          width = (double) (*(obj->laneWidth));
        }

        if (obj->approach != NULL) {
          i += BuildLanes(&(lane_tmp[i]), INBOUND, obj->approach, width);
        }

        if (obj->egress != NULL) {
          i += BuildLanes(&(lane_tmp[i]), OUTBOUND, obj->egress, width);
        }
      }
      lane_manager->lane_count = lane_count;
      lane_manager->lanes = lane_tmp;
      lane_manager->intersection_id = BuildInt(intersect->id.buf, 0,
          intersect->id.size);
      tmp = (LaneManagerList*) malloc(sizeof(LaneManagerList));
      tmp->lane_manager = lane_manager;
      tmp->next = lane_managers;
      lane_managers = tmp;
    }

  } else {

    return NULL;
  }

  asn_DEF_MapData.free_struct(&asn_DEF_MapData, map_dat, 0);

  return lane_managers;
}

/**
 * Returns a pointer to a list of lane nodes parsed from the given list of map
 * data nodes and the default width. The default width is only used if no
 * width is found in the bytes for a particular node being parsed.
 *
 * @param nodes a pointer to the list of map data nodes
 * @param width the default lane width
 * @return a pointer to the list of parsed lane nodes
 */
static LaneNode *ParseNodes(NodeList_t *nodes, double width) {

  LaneNode *node = (LaneNode *) malloc(sizeof(LaneNode) * nodes->list.count);
  int i;
  for (i = 0; i < nodes->list.count; i++) {

    ParseOctetToNode(nodes->list.array[i], &(node[i]), width);

    if (i > 0) {

      node[i].x += node[i - 1].x;
      node[i].y += node[i - 1].y;
      node[i].z += node[i - 1].z;
    }
  }

  return node;
}

/**
 * Updates the given lane node with data parsed from the given byte string.
 *
 * @param octet a pointer to the byte string to parse
 * @param ret a pointer to the lane node to update with the parsed data
 * @param width the default width to use if no width can be parsed
 */
static void ParseOctetToNode(
    OCTET_STRING_t *octet, LaneNode *node, double width) {

  node->x = (double) ConvertToInt16(octet->buf, 0);
  node->y = (double) ConvertToInt16(octet->buf, 2);

  if (octet->size > 4) {

    node->z = (double) ConvertToInt16(octet->buf, 4);

  } else {

    node->z = 0.0;
  }

  if (octet->size > 6) {

    node->width = (double) ConvertToInt16(octet->buf, 6);

  } else {

    node->width = width;
  }
}

/**
 * Returns a pointer to a list of signal indications parsed from the given
 * SPaT message.
 *
 * @param bytes a pointer to the SPaT message
 * @param size the length (number of bytes) of the SPaT message
 * @return a pointer to the parsed list of signal indications
 */
SignalIndicationList *ParseSpat(char *bytes, int size) {

  SPAT_t *spat = 0;
  asn_dec_rval_t rval;
  SignalIndicationList *signals = 0;

  rval = asn_DEF_SPAT.ber_decoder(0, &asn_DEF_SPAT, (void **) &spat, bytes,
      size, 0);

  if (rval.code == RC_OK) {

    int i;
    for (i = 0; i < spat->intersections.list.count; i++) {
      signals = ParseSpatIntersection(
          spat->intersections.list.array[i], signals);
    }
  }

  asn_DEF_SPAT.free_struct(&asn_DEF_SPAT, spat, 0);

  return signals;
}

/**
 * Returns a pointer to a signal indication list updated with the given list
 * of intersection states.
 *
 * @param intersection a pointer to the list of intersection states
 * @param signals a pointer to the list of signal indications to update
 * @param a pointer to the updated list of signal indications
 */
static SignalIndicationList *ParseSpatIntersection(
    IntersectionState_t *intersection, SignalIndicationList *signals) {

  SignalIndicationList* list = signals;

  int i;
  for (i = 0; i < intersection->states.list.count; i++) {
    list = ParseSpatMovementState(ParseSpatIntersectionId(intersection->id),
        ParseSpatIntersectionStatusObject(intersection->status),
        intersection->states.list.array[i], list);
  }

  return list;
}

/**
 * Returns the integer ID parsed from the given intersection ID.
 *
 * @param id the intersection ID.
 * @return the parsed integer ID.
 */
static int ParseSpatIntersectionId(IntersectionID_t id) {

  return BuildInt(id.buf, 0, id.size);
}

/**
 * Returns the integer status parsed from the given intersection status
 * object.
 *
 * @param status_object the intersection status object
 * @return the parsed integer status.
 */
static int ParseSpatIntersectionStatusObject(
    IntersectionStatusObject_t status_object) {

  return BuildInt(status_object.buf, 0, status_object.size);
}

/**
 * Returns a pointer to a signal indication list updated width the given
 * intersection information and movement states.
 *
 * @param intersection_id the ID of the relevant intersection
 * @param intersection_status the status of the relevant intersection
 * @param movement a pointer to the movement state
 * @param signals a pointer to the list of signal indications to update
 * @return a pointer to the updated signal indications list
 */
static SignalIndicationList *ParseSpatMovementState(
    int intersection_id, int intersection_status, MovementState_t *movement,
    SignalIndicationList *signals) {

  if (movement->currState == NULL) {
    return signals;
  }

  int i;
  for (i = 0; i < movement->laneSet.size; i++) {
    signals = ParseSpatSignalIndication(*(movement->currState), signals,
        (int) movement->laneSet.buf[i], intersection_id,
        movement->timeToChange / 10.0);
  }

  return signals;
}

/**
 * Returns a pointer to a signal indication list updated with a parsed signal
 * indication.
 *
 * @param indication the signal indication bytes to parse
 * @param signals a pointer to the list of existing signal indications
 * @param lane_id the ID of the lane the signal indication affects
 * @param intersection_id the ID of the intersection where the signal exists
 * @param time_to_change the time for the signal to change
 * @return a pointer to the updated signal indication list
 */
static SignalIndicationList *ParseSpatSignalIndication(
    long indication, SignalIndicationList *signals, int lane_id,
    int intersection_id, double time_to_change) {

  SignalIndicationList* list = signals;

  if (indication == 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = NONE;
    tmp->signal->type = UNKNOWN;
    tmp->signal->state = SOFT;
    tmp->next = list;
    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    list = tmp;
  }

  if ((indication & 0x1) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = GREEN;
    tmp->signal->type = BALL;
    tmp->signal->state = STEADY;

    if ((indication & 0x8) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = signals;
    list = tmp;
  }

  if ((indication & 0x10) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = GREEN;
    tmp->signal->type = LEFT_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x80) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x100) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = GREEN;
    tmp->signal->type = RIGHT_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x800) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x1000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = GREEN;
    tmp->signal->type = STRAIGHT_ARROW;
    tmp->signal->state = STEADY;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x10000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = GREEN;
    tmp->signal->type = LEFT_ARROW;
    tmp->signal->state = SOFT;
    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x100000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = GREEN;
    tmp->signal->type = RIGHT_ARROW;
    tmp->signal->state = SOFT;
    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x1000000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = GREEN;
    tmp->signal->type = UTURN_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x8000000) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x2) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = YELLOW;
    tmp->signal->type = BALL;
    tmp->signal->state = STEADY;

    if ((indication & 0x8) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x20) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = YELLOW;
    tmp->signal->type = LEFT_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x80) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x200) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = YELLOW;
    tmp->signal->type = RIGHT_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x800) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x2000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = YELLOW;
    tmp->signal->type = STRAIGHT_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x8000) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x20000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = YELLOW;
    tmp->signal->type = LEFT_ARROW;
    tmp->signal->state = SOFT;
    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x200000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = YELLOW;
    tmp->signal->type = RIGHT_ARROW;
    tmp->signal->state = SOFT;
    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x2000000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = YELLOW;
    tmp->signal->type = UTURN_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x8000000) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x4) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = RED;
    tmp->signal->type = BALL;
    tmp->signal->state = STEADY;

    if ((indication & 0x8) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x40) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = RED;
    tmp->signal->type = LEFT_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x80) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x400) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = RED;
    tmp->signal->type = RIGHT_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x800) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = signals;
    list = tmp;
  }

  if ((indication & 0x4000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = RED;
    tmp->signal->type = STRAIGHT_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x8000) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x40000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = RED;
    tmp->signal->type = LEFT_ARROW;
    tmp->signal->state = SOFT;
    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x400000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = RED;
    tmp->signal->type = RIGHT_ARROW;
    tmp->signal->state = SOFT;
    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  if ((indication & 0x4000000) != 0) {

    SignalIndicationList *tmp = (SignalIndicationList *) malloc(
        sizeof(SignalIndicationList));
    tmp->signal = (SignalIndication*) malloc(sizeof(SignalIndication));
    tmp->signal->color = RED;
    tmp->signal->type = UTURN_ARROW;
    tmp->signal->state = STEADY;

    if ((indication & 0x8000000) != 0) {
      tmp->signal->state = FLASHING;
    }

    tmp->signal->lane_id = lane_id;
    tmp->signal->intersection_id = intersection_id;
    tmp->signal->time_to_change = time_to_change;
    tmp->next = list;
    list = tmp;
  }

  return list;
}

/*
 * Returns whether the given point is in the given lane.
 *
 * @param x the x coordinate of the point to test
 * @param y the y coordinate of the point to test
 * @param lane a pointer to the lane to test for containment
 * @return a value of 1 (true) if the given point is in the given lane,
 * otherwise a value of 0 (false) is returned
 */
static int PointInLane(double x, double y, Lane *lane) {

  /* TODO: ablatt - we are straight ignoring the center of the intersection
   * with this algorithm...
   */

  if (lane->type == OUTBOUND) {

    return 0; // we don't care about outbound lanes
  }

  int i;
  for (i = 0; i < lane->node_count - 1; i++) {
    if (PointInLaneNodes(x, y, &(lane->nodes[i]), &(lane->nodes[i + 1]))) {
      return 1;
    }
    //if statement is to see if the point is within the radius of a middle node
    //which is why the first and last nodes are not checked
    else if (i < lane->node_count - 2) {
      if (PointWithinRadius(x, y, &(lane->nodes[i + 1]))) {
        return 1;
      }
    }
  }

  return 0;
}

/*
 * Returns whether the given point is in the polygon derived from the given
 * lane nodes.
 *
 * @param x the x coordinate of the point to test
 * @param y the y coordinate of the point to test
 * @param node1 a pointer to the first lane node
 * @param node2 a pointer to the second lane node
 * @return a value of 1 (true) if the given point is in the polygon derived
 * from the given lane nodes, otherwise a value of 0 (false) is returned
 */
static int PointInLaneNodes(
    double x, double y, LaneNode *node1, LaneNode *node2) {

  struct Polygon *p = BuildPolygonFromNodes(node1, node2);
  int contained = PointInPoly(4, x, y, p->xs, p->ys);
  free(p->xs);
  free(p->ys);
  free(p);
  return contained;
}

/**
 * Returns whether the given point is in the radius of the node specified by the given
 * width and location of the node.
 *
 * @param x the x coordinate of the point to test
 * @param y the y coordinate of the point to test
 * @param node the node to check
 * @return a value of 1 (true) if the given coordinates are in the node's radius,
 * otherwise a value of 0 (false) is returned
 */
static int PointWithinRadius(double x, double y, LaneNode *node) {

  double radius = (node->width) / 2;
  double distance = (node->x - x) * (node->x - x)
      + (node->y - y) * (node->y - y);
  if (radius * radius >= distance) {
    return 1;
  } else {
    return 0;
  }
}

/**
 * Returns whether the given point is in the polygon specified by the given
 * arrays of x and y coordinates for the vertices. This function uses a 
 * test to see how many times a ray, starting from the point and going in any 
 * fixed direction, intersects the edges of the polygon. If the point is on 
 * the outside of the polygon the ray will intersect its edge an even number 
 * of times. If the point is on the inside of the polygon then it will 
 * intersect the edge an odd number of times.
 *
 * @param vertex_count the number of vertices in the polygon
 * @param testx the x coordinate of the point to test
 * @param testy the y coordinate of the point to test
 * @param vertx a pointer to the array of x coordinates for polygon vertices
 * @param verty a pointer to the array of y coordinates for polygon vertices
 * @return a value of 1 (true) if the given coordinates are in the polygon
 * specified by the given arrays of vertex coordinates, otherwise a value of 0
 * (false) is returned
 */
static int PointInPoly(
    int vertex_count, double testx, double testy, double *vertx,
    double *verty) {

  int i, j, contained = 0;

  // iterate through the polygon 2 vertices at a time (vertex i and vertex j)
  for (i = 0, j = vertex_count - 1; i < vertex_count; j = i++) {

    // check to see if a ray extending from the test point along the positive x
    // direction crosses the current vertex of the polygon

    // first, cull out all polygon segments that are completely left of the x of 
    // the test point (where left is the negative x axis)
    if (vertx[i] > testx && vertx[j] > testx) {

      // then, cull out all polygon segments that are completely above or below
      // the y of the test point (where up and down are the y axis)
      // this will also ensure that we are not testing against a horizontal line
      if ((verty[i] > testy) != (verty[j] > testy)) {

        // test the y component of the polygon segment, if positive check for
        // less than, if negative check for greater than
        // also, verty == 0 should never happen because of the check for
        // horizontal lines beforehand
        int ydiff = (verty[j] - verty[i]);
        if (ydiff >= 0) {
          if ((testx - vertx[i]) * ydiff < (vertx[j] - vertx[i]) *
              (testy - verty[i])) {

            // if the ray crosses a vertex, then change the current state of contained
            // return variable because if the ray crosses an even number of times then
            // the test point is outside of the polygon and if it crosses an odd
            // number of times then the test point is inside the polygon. 
            contained = !contained;
          }
        } else {
          if ((testx - vertx[i]) * ydiff > (vertx[j] - vertx[i]) *
              (testy - verty[i])) {

            // if the ray crosses a vertex, then change the current state of contained
            // return variable because if the ray crosses an even number of times then
            // the test point is outside of the polygon and if it crosses an odd
            // number of times then the test point is inside the polygon. 
            contained = !contained;
          }
        }
      }
    }
  }

  return contained;
}

/**
 * Returns whether a given vehicle is within the BSM filtering range.
 *
 * @param vehicle a pointer to the vehicle to test
 * @return a value of 1 (true) if the vehicle is in the BSM filtering range,
 * otherwise a value of 0 (false) is returned
 */
static int VehicleInRange(Vehicle *vehicle) {

  /*
   * NOTE: the reference point is (0, 0), so the distance of the vehicle from
   * the intersection can be calculated using only the x and y of the vehicle.
   */

  // get the distance of the vehicle from the intersection
  double distance_squared = (vehicle->x * vehicle->x)
      + (vehicle->y * vehicle->y);

  // return whether the vehicle is within the BSM filtering range
  return distance_squared <= GetSquaredBsmRadius();
}
