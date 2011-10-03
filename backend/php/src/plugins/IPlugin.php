<?php
/**
 * All functions must be implemented to create a correct POG plugin
 * The 'optional' functions SetupRender() and AuthorPage() must be implemented and return null
 * if your plugin does not need them.
 *
 */
interface POG_Plugin
{
	/**
	 *
	 * REQUIRED
	 * This function must return the version of the plugin.
	 * It will be used to automatically notify developer when your plugin is updated.
	 *
	 */
	function Version();

	/**
	 *
	 * REQUIRED
	 * This function performs the actions that your plugin provides.
	 *
	 */
	function Execute();

	/**
	 * 'OPTIONAL'
	 * If your plugin needs an administrative interface, implement this function.
	 * Else return null
	 *
	 */
	function SetupRender();


	/**
	 *
	 * 'OPTIONAL'
	 * Implement this function to provide a link to your homepage.
	 * e.g. return 'http://myhomepage.com';
	 *
	 * return null if you do not want to link to your homepage
	 * e.g. return null
	 *
	 */
	function AuthorPage();
}
?>