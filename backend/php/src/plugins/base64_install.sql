-- base64.sql - MySQL base64 encoding/decoding functions
-- Copyright (C) 2006 Ian Gulliver
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of version 2 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


DROP TABLE IF EXISTS base64_data |
CREATE TABLE base64_data (c CHAR(1) BINARY, val TINYINT) |
INSERT INTO base64_data VALUES
	('A',0), ('B',1), ('C',2), ('D',3), ('E',4), ('F',5), ('G',6), ('H',7), ('I',8), ('J',9),
	('K',10), ('L',11), ('M',12), ('N',13), ('O',14), ('P',15), ('Q',16), ('R',17), ('S',18), ('T',19),
	('U',20), ('V',21), ('W',22), ('X',23), ('Y',24), ('Z',25), ('a',26), ('b',27), ('c',28), ('d',29),
	('e',30), ('f',31), ('g',32), ('h',33), ('i',34), ('j',35), ('k',36), ('l',37), ('m',38), ('n',39),
	('o',40), ('p',41), ('q',42), ('r',43), ('s',44), ('t',45), ('u',46), ('v',47), ('w',48), ('x',49),
	('y',50), ('z',51), ('0',52), ('1',53), ('2',54), ('3',55), ('4',56), ('5',57), ('6',58), ('7',59),
	('8',60), ('9',61), ('+',62), ('/',63), ('=',0) |


DROP FUNCTION IF EXISTS BASE64_DECODE |
CREATE FUNCTION BASE64_DECODE (input BLOB)
	RETURNS BLOB
	CONTAINS SQL
	DETERMINISTIC
	SQL SECURITY INVOKER
BEGIN
	DECLARE ret BLOB DEFAULT '';
	DECLARE done TINYINT DEFAULT 0;

	IF input IS NULL THEN
		RETURN NULL;
	END IF;

each_block:
	WHILE NOT done DO BEGIN
		DECLARE accum_value BIGINT UNSIGNED DEFAULT 0;
		DECLARE in_count TINYINT DEFAULT 0;
		DECLARE out_count TINYINT DEFAULT 3;

each_input_char:
		WHILE in_count < 4 DO BEGIN
			DECLARE first_char CHAR(1);

			IF LENGTH(input) = 0 THEN
				RETURN ret;
			END IF;

			SET first_char = SUBSTRING(input,1,1);
			SET input = SUBSTRING(input,2);

			BEGIN
				DECLARE tempval TINYINT UNSIGNED;
				DECLARE error TINYINT DEFAULT 0;
				DECLARE base64_getval CURSOR FOR SELECT val FROM base64_data WHERE c = first_char;
				DECLARE CONTINUE HANDLER FOR SQLSTATE '02000' SET error = 1;

				OPEN base64_getval;
				FETCH base64_getval INTO tempval;
				CLOSE base64_getval;

				IF error THEN
					ITERATE each_input_char;
				END IF;

				SET accum_value = (accum_value << 6) + tempval;
			END;

			SET in_count = in_count + 1;

			IF first_char = '=' THEN
				SET done = 1;
				SET out_count = out_count - 1;
			END IF;
		END; END WHILE;

		-- We've now accumulated 24 bits; deaccumulate into bytes

		-- We have to work from the left, so use the third byte position and shift left
		WHILE out_count > 0 DO BEGIN
			SET ret = CONCAT(ret,CHAR((accum_value & 0xff0000) >> 16));
			SET out_count = out_count - 1;
			SET accum_value = (accum_value << 8) & 0xffffff;
		END; END WHILE;

	END; END WHILE;

	RETURN ret;
END |

DROP FUNCTION IF EXISTS BASE64_ENCODE |
CREATE FUNCTION BASE64_ENCODE (input BLOB)
	RETURNS BLOB
	CONTAINS SQL
	DETERMINISTIC
	SQL SECURITY INVOKER
BEGIN
	DECLARE ret BLOB DEFAULT '';
	DECLARE done TINYINT DEFAULT 0;

	IF input IS NULL THEN
		RETURN NULL;
	END IF;

each_block:
	WHILE NOT done DO BEGIN
		DECLARE accum_value BIGINT UNSIGNED DEFAULT 0;
		DECLARE in_count TINYINT DEFAULT 0;
		DECLARE out_count TINYINT;

each_input_char:
		WHILE in_count < 3 DO BEGIN
			DECLARE first_char CHAR(1);

			IF LENGTH(input) = 0 THEN
				SET done = 1;
				SET accum_value = accum_value << (8 * (3 - in_count));
				LEAVE each_input_char;
			END IF;

			SET first_char = SUBSTRING(input,1,1);
			SET input = SUBSTRING(input,2);

			SET accum_value = (accum_value << 8) + ASCII(first_char);

			SET in_count = in_count + 1;
		END; END WHILE;

		-- We've now accumulated 24 bits; deaccumulate into base64 characters

		-- We have to work from the left, so use the third byte position and shift left
		CASE
			WHEN in_count = 3 THEN SET out_count = 4;
			WHEN in_count = 2 THEN SET out_count = 3;
			WHEN in_count = 1 THEN SET out_count = 2;
			ELSE RETURN ret;
		END CASE;

		WHILE out_count > 0 DO BEGIN
			BEGIN
				DECLARE out_char CHAR(1);
				DECLARE base64_getval CURSOR FOR SELECT c FROM base64_data WHERE val = (accum_value >> 18);

				OPEN base64_getval;
				FETCH base64_getval INTO out_char;
				CLOSE base64_getval;

				SET ret = CONCAT(ret,out_char);
				SET out_count = out_count - 1;
				SET accum_value = accum_value << 6 & 0xffffff;
			END;
		END; END WHILE;

		CASE
			WHEN in_count = 2 THEN SET ret = CONCAT(ret,'=');
			WHEN in_count = 1 THEN SET ret = CONCAT(ret,'==');
			ELSE BEGIN END;
		END CASE;

	END; END WHILE;

	RETURN ret;
END |
