<?php
/* $Id: read_dump.lib.php,v 2.11 2006/01/17 17:02:30 cybot_tm Exp $ */
// vim: expandtab sw=4 ts=4 sts=4:

/**
 * Removes comment lines and splits up large sql files into individual queries
 *
 * Last revision: September 23, 2001 - gandon
 *
 * @param   array    the splitted sql commands
 * @param   string   the sql commands
 * @param   integer  the MySQL release number (because certains php3 versions
 *                   can't get the value of a constant from within a function)
 *
 * @return  boolean  always true
 *
 * @access  public
 */
function PMA_splitSqlFile(&$ret, $sql, $release)
{
    // do not trim, see bug #1030644
    //$sql          = trim($sql);
    $sql          = rtrim($sql, "\n\r");
    $sql_len      = strlen($sql);
    $char         = '';
    $string_start = '';
    $in_string    = FALSE;
    $nothing      = TRUE;
    $time0        = time();

    for ($i = 0; $i < $sql_len; ++$i) {
        $char = $sql[$i];

        // We are in a string, check for not escaped end of strings except for
        // backquotes that can't be escaped
        if ($in_string) {
            for (;;) {
                $i         = strpos($sql, $string_start, $i);
                // No end of string found -> add the current substring to the
                // returned array
                if (!$i) {
                    $ret[] = array('query' => $sql, 'empty' => $nothing);
                    return TRUE;
                }
                // Backquotes or no backslashes before quotes: it's indeed the
                // end of the string -> exit the loop
                elseif ($string_start == '`' || $sql[$i-1] != '\\') {
                    $string_start      = '';
                    $in_string         = FALSE;
                    break;
                }
                // one or more Backslashes before the presumed end of string...
                else {
                    // ... first checks for escaped backslashes
                    $j                     = 2;
                    $escaped_backslash     = FALSE;
                    while ($i-$j > 0 && $sql[$i-$j] == '\\') {
                        $escaped_backslash = !$escaped_backslash;
                        $j++;
                    }
                    // ... if escaped backslashes: it's really the end of the
                    // string -> exit the loop
                    if ($escaped_backslash) {
                        $string_start  = '';
                        $in_string     = FALSE;
                        break;
                    }
                    // ... else loop
                    else {
                        $i++;
                    }
                } // end if...elseif...else
            } // end for
        } // end if (in string)

        // lets skip comments (/*, -- and #)
        elseif (($char == '-' && $sql_len > $i + 2 && $sql[$i + 1] == '-' && $sql[$i + 2] <= ' ') || $char == '#' || ($char == '/' && $sql_len > $i + 1 && $sql[$i + 1] == '*')) {
            $i = strpos($sql, $char == '/' ? '*/' : "\n", $i);
            // didn't we hit end of string?
            if ($i === FALSE) {
                break;
            }
            if ($char == '/') {
                $i++;
            }
        }

        // We are not in a string, first check for delimiter...
        elseif ($char == ';') {
            // if delimiter found, add the parsed part to the returned array
            $ret[]      = array('query' => substr($sql, 0, $i), 'empty' => $nothing);
            $nothing    = TRUE;
            $sql        = ltrim(substr($sql, min($i + 1, $sql_len)));
            $sql_len    = strlen($sql);
            if ($sql_len) {
                $i      = -1;
            } else {
                // The submited statement(s) end(s) here
                return TRUE;
            }
        } // end elseif (is delimiter)

        // ... then check for start of a string,...
        elseif (($char == '"') || ($char == '\'') || ($char == '`')) {
            $in_string    = TRUE;
            $nothing      = FALSE;
            $string_start = $char;
        } // end elseif (is start of string)

        elseif ($nothing) {
            $nothing = FALSE;
        }

        // loic1: send a fake header each 30 sec. to bypass browser timeout
        $time1     = time();
        if ($time1 >= $time0 + 30) {
            $time0 = $time1;
            header('X-pmaPing: Pong');
        } // end if
    } // end for

    // add any rest to the returned array
    if (!empty($sql) && preg_match('@[^[:space:]]+@', $sql)) {
        $ret[] = array('query' => $sql, 'empty' => $nothing);
    }

    return TRUE;
} // end of the 'PMA_splitSqlFile()' function


/**
 * Reads (and decompresses) a (compressed) file into a string
 *
 * @param   string   the path to the file
 * @param   string   the MIME type of the file, if empty MIME type is autodetected
 *
 * @global  array    the phpMyAdmin configuration
 *
 * @return  string   the content of the file or
 *          boolean  FALSE in case of an error.
 */
function PMA_readFile($path, $mime = '') {
    global $cfg;

    if (!file_exists($path)) {
        return FALSE;
    }
    switch ($mime) {
        case '':
            $file = @fopen($path, 'rb');
            if (!$file) {
                return FALSE;
            }
            $test = fread($file, 3);
            fclose($file);
            if ($test[0] == chr(31) && $test[1] == chr(139)) {
                return PMA_readFile($path, 'application/x-gzip');
            }
            if ($test == 'BZh') {
                return PMA_readFile($path, 'application/x-bzip');
            }
            return PMA_readFile($path, 'text/plain');
        case 'text/plain':
            $file = @fopen($path, 'rb');
            if (!$file) {
                return FALSE;
            }
            $content = fread($file, filesize($path));
            fclose($file);
            break;
        case 'application/x-gzip':
            if ($cfg['GZipDump'] && @function_exists('gzopen')) {
                $file = @gzopen($path, 'rb');
                if (!$file) {
                    return FALSE;
                }
                $content = '';
                while (!gzeof($file)) {
                    $content .= gzgetc($file);
                }
                gzclose($file);
            } else {
                return FALSE;
            }
           break;
        case 'application/x-bzip':
            if ($cfg['BZipDump'] && @function_exists('bzdecompress')) {
                $file = @fopen($path, 'rb');
                if (!$file) {
                    return FALSE;
                }
                $content = fread($file, filesize($path));
                fclose($file);
                $content = bzdecompress($content);
            } else {
                return FALSE;
            }
           break;
        default:
           return FALSE;
    }
    return $content;
}

?>
