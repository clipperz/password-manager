'strict';

function currentCommit() {
    return process.env.CURRENT_COMMIT;
}

function shareURL() {
	return process.env.SHARE_URL
}

function redeemURL() {
	return process.env.REDEEM_URL
}

function appURL() {
	return process.env.APP_URL
}

export { currentCommit, shareURL, redeemURL, appURL };
