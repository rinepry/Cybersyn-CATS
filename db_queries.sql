-- Current vehicle status
CREATE VIEW current_vehicle_status AS
select 
vp.vehicle_id, 
vp.lat, 
vp.lon, 
vp.trip_id,  
vp.route_id, 
r.route_long_name, 
t.trip_headsign, 
vp.timestamp
from vehicle_positions vp 
inner join routes r on r.route_id = vp.route_id
inner join trips t on t.trip_id =  vp.trip_id

-- All trips by stops & stop times
CREATE VIEW stop_trips AS
select 
st.stop_id,
r.route_id,
max(s.stop_name) as stop_name,
max(r.route_long_name) as route_name,
max(t.trip_headsign) as headsign,  
max(st.trip_id) as trip_id, 
max(st.arrival_time) as arrival_time_epoch, 
max(t.direction_id) as direction
from stop_times st
inner join stops s on st.stop_id = s.stop_id
inner join trips t on st.trip_id = t.trip_id
inner join routes r on t.route_id = r.route_id
group by st.stop_id, r.route_id
