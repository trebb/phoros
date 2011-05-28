-- PHOROS -- Photogrammetric Road Survey
-- Copyright (C) 2011 Bert Burgemeister
-- 
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
----------------------------------------------------------------------


-- This is a working example for a PL/pgSQL trigger function body.
-- Once deployed, it is fired on changes to the user point table.  It
-- can be deployed with your presentation project like this:
--
-- ./phoros --redefine-trigger-function=<your-presentation-project> --plpgsql-body=<name-of-this-file> ...
--
-- The code threads points from the user point table, grouped by
-- column description, to linestrings which are stored into the user
-- line table.  The strings ~0@*~A and ~1@*~A are placeholders for the
-- names of user point table and user line table, respectively.

DECLARE
  polyline GEOMETRY;
  new_point point_bag%ROWTYPE;
  tried_point point_bag%ROWTYPE;
  previous_point point_bag%ROWTYPE;
  starting_point GEOMETRY;
  point_group RECORD;
  -- Set max_bend = 180 in order not to discard any points, and to get
  -- crazy zigzag lines.
  max_bend DOUBLE PRECISION DEFAULT 91; -- degrees

BEGIN

  -- Muffle warnings about implicitly created stuff:
  SET client_min_messages TO ERROR;

  CREATE TEMPORARY TABLE point_bag
    (id SERIAL primary key, coordinates GEOMETRY, description TEXT)
    ON COMMIT DROP;

  <<thread_one_line>>
  FOR point_group
  IN SELECT description FROM ~0@*~A GROUP BY description
  LOOP

    INSERT INTO point_bag (coordinates, description)
      SELECT coordinates, description
        FROM ~0@*~A
        WHERE attribute = 'polyline'
              AND
              description = point_group.description;
  
    starting_point :=
      (SELECT st_centroid(st_collect(coordinates))
         FROM point_bag);
  
    previous_point := 
      (SELECT ROW(id, coordinates) 
         FROM point_bag 
         ORDER BY st_distance(point_bag.coordinates, starting_point)
         LIMIT 1);
  
    DELETE FROM point_bag WHERE id = previous_point.id;
  
    new_point := 
      (SELECT ROW(id, coordinates)
         FROM point_bag
         ORDER BY st_distance(point_bag.coordinates,
                              previous_point.coordinates)
         LIMIT 1);
  
    polyline := st_makeline(previous_point.coordinates,
                        new_point.coordinates);
  
    DELETE FROM point_bag WHERE id = new_point.id;
  
    <<add_or_discard_point>>
    LOOP
      previous_point.coordinates := st_pointn(polyline,1);
  
      new_point :=
        (SELECT ROW(id, coordinates)
           FROM point_bag
           ORDER BY st_distance(coordinates, previous_point.coordinates)
           LIMIT 1);
  
      EXIT WHEN new_point IS NULL;
  
      IF bendedness(st_pointn(polyline, 2),
                    st_pointn(polyline, 1), 
                    new_point.coordinates)
         < bendedness(st_pointn(polyline, st_npoints(polyline) - 1), 
                      st_pointn(polyline, st_npoints(polyline)),
                      new_point.coordinates)
         AND
         bendedness(st_pointn(polyline, 2), st_pointn(polyline, 1),
                     new_point.coordinates)
         < radians(max_bend)
      THEN
          polyline := st_addpoint(polyline, new_point.coordinates, 0);
          DELETE FROM point_bag WHERE id = new_point.id;
      END IF;
  
      polyline := st_reverse(polyline);
  
      DELETE FROM point_bag WHERE id = tried_point.id;
  
      tried_point := new_point;
    END LOOP add_or_discard_point;
  
    -- RAISE NOTICE '%', st_astext(polyline);

    DELETE FROM ~1@*~A
           WHERE description = point_group.description;

    INSERT INTO ~1@*~A
           (description, line) VALUES (point_group.description, polyline);

  END LOOP thread_one_line;
END;
