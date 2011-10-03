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
DROP FUNCTION IF EXISTS BASE64_DECODE |
DROP FUNCTION IF EXISTS BASE64_ENCODE |