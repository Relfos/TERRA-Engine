Unit SteamAPI;

{$IFDEF FPC} {$MODE DELPHI} {$ENDIF}

Interface
Uses {$IFDEF MSWINDOWS}Windows{$ELSE}DynLibs{$ENDIF};

Const
{$IFDEF MSWINDOWS}
  SteamWrapperName = 'CSteamworks.dll';
{$ENDIF}

{$IFDEF DARWIN}
  SteamWrapperName = 'CSteamworks.bundle/Contents/MacOS/CSteamworks';
{$ENDIF}

{$IFDEF LINUX}
  SteamWrapperName = 'libCSteamworks.so';
{$ENDIF}

Type
	SteamID = UInt64;
	PSteamID = ^SteamID;
	SteamAppID = Cardinal;
	PSteamAppID = ^SteamAppID;
	SteamAccountID = Cardinal;
	PSteamAccountID = ^SteamAccountID;
	SteamDepotID = Cardinal;
	PSteamDepotID = ^SteamDepotID;
	SteamGameID = UInt64;
	PSteamGameID = ^SteamGameID;
	SteamAPICall = UInt64;
	PSteamAPICall = ^SteamAPICall;
	SteamPublishedFileId = UInt64;
	PSteamPublishedFileId = ^SteamPublishedFileId;
	SteamSNetSocket = Cardinal;
	PSteamSNetSocket = ^SteamSNetSocket;
	SteamSNetListenSocket = Cardinal;
	PSteamSNetListenSocket = ^SteamSNetListenSocket;
	SteamLeaderboard = UInt64;
	PSteamLeaderboard = ^SteamLeaderboard;
	SteamLeaderboardEntries = UInt64;
	PSteamLeaderboardEntries = ^SteamLeaderboardEntries;

	SteamPipeHandle = Integer;
	SteamUserHandle = Integer;
	SteamAuthTicketHandle = Integer;
	SteamHTTPRequestHandle = Integer;
	SteamHTMLBrowserHandle = Integer;
	SteamServerListRequestHandle = Integer;
	SteamServerQueryHandle = Integer;
	SteamUGCHandle = Integer;
	SteamUGCUpdateHandle = Integer;
	SteamUGCQueryHandle = Integer;
	SteamUGCFileWriteStreamHandle = Integer;
	SteamPublishedFileUpdateHandle = Integer;
	SteamScreenshotHandle = Integer;
	SteamClientUnifiedMessageHandle = Integer;

Const
	STEAMAPPLIST_INTERFACE_VERSION:PAnsiChar = 'STEAMAPPLIST_INTERFACE_VERSION001';
	STEAMAPPS_INTERFACE_VERSION:PAnsiChar = 'STEAMAPPS_INTERFACE_VERSION006';
	STEAMAPPTICKET_INTERFACE_VERSION:PAnsiChar = 'STEAMAPPTICKET_INTERFACE_VERSION001';
	STEAMCLIENT_INTERFACE_VERSION:PAnsiChar = 'SteamClient016';
	STEAMCONTROLLER_INTERFACE_VERSION:PAnsiChar = 'STEAMCONTROLLER_INTERFACE_VERSION';
	STEAMFRIENDS_INTERFACE_VERSION:PAnsiChar = 'SteamFriends014';
	STEAMGAMECOORDINATOR_INTERFACE_VERSION:PAnsiChar = 'SteamGameCoordinator001';
	STEAMGAMESERVER_INTERFACE_VERSION:PAnsiChar = 'SteamGameServer012';
	STEAMGAMESERVERSTATS_INTERFACE_VERSION:PAnsiChar = 'SteamGameServerStats001';
	STEAMHTMLSURFACE_INTERFACE_VERSION:PAnsiChar = 'STEAMHTMLSURFACE_INTERFACE_VERSION_002';
	STEAMHTTP_INTERFACE_VERSION:PAnsiChar = 'STEAMHTTP_INTERFACE_VERSION002';
	STEAMMATCHMAKING_INTERFACE_VERSION:PAnsiChar = 'SteamMatchMaking009';
	STEAMMATCHMAKINGSERVERS_INTERFACE_VERSION:PAnsiChar = 'SteamMatchMakingServers002';
	STEAMMUSIC_INTERFACE_VERSION:PAnsiChar = 'STEAMMUSIC_INTERFACE_VERSION001';
	STEAMMUSICREMOTE_INTERFACE_VERSION:PAnsiChar = 'STEAMMUSICREMOTE_INTERFACE_VERSION001';
	STEAMNETWORKING_INTERFACE_VERSION:PAnsiChar = 'SteamNetworking005';
	STEAMREMOTESTORAGE_INTERFACE_VERSION:PAnsiChar = 'STEAMREMOTESTORAGE_INTERFACE_VERSION012';
	STEAMSCREENSHOTS_INTERFACE_VERSION:PAnsiChar = 'STEAMSCREENSHOTS_INTERFACE_VERSION002';
	STEAMUGC_INTERFACE_VERSION:PAnsiChar = 'STEAMUGC_INTERFACE_VERSION003';
	STEAMUNIFIEDMESSAGES_INTERFACE_VERSION:PAnsiChar = 'STEAMUNIFIEDMESSAGES_INTERFACE_VERSION001';
	STEAMUSER_INTERFACE_VERSION:PAnsiChar = 'SteamUser017';
	STEAMUSERSTATS_INTERFACE_VERSION:PAnsiChar = 'STEAMUSERSTATS_INTERFACE_VERSION011';
	STEAMUTILS_INTERFACE_VERSION:PAnsiChar = 'SteamUtils007';
	k_cubAppProofOfPurchaseKeyMax = 64;
	k_iSteamUserCallbacks = 100;
	k_iSteamGameServerCallbacks = 200;
	k_iSteamFriendsCallbacks = 300;
	k_iSteamBillingCallbacks = 400;
	k_iSteamMatchmakingCallbacks = 500;
	k_iSteamContentServerCallbacks = 600;
	k_iSteamUtilsCallbacks = 700;
	k_iClientFriendsCallbacks = 800;
	k_iClientUserCallbacks = 900;
	k_iSteamAppsCallbacks = 1000;
	k_iSteamUserStatsCallbacks = 1100;
	k_iSteamNetworkingCallbacks = 1200;
	k_iClientRemoteStorageCallbacks = 1300;
	k_iClientDepotBuilderCallbacks = 1400;
	k_iSteamGameServerItemsCallbacks = 1500;
	k_iClientUtilsCallbacks = 1600;
	k_iSteamGameCoordinatorCallbacks = 1700;
	k_iSteamGameServerStatsCallbacks = 1800;
	k_iSteam2AsyncCallbacks = 1900;
	k_iSteamGameStatsCallbacks = 2000;
	k_iClientHTTPCallbacks = 2100;
	k_iClientScreenshotsCallbacks = 2200;
	k_iSteamScreenshotsCallbacks = 2300;
	k_iClientAudioCallbacks = 2400;
	k_iClientUnifiedMessagesCallbacks = 2500;
	k_iSteamStreamLauncherCallbacks = 2600;
	k_iClientControllerCallbacks = 2700;
	k_iSteamControllerCallbacks = 2800;
	k_iClientParentalSettingsCallbacks = 2900;
	k_iClientDeviceAuthCallbacks = 3000;
	k_iClientNetworkDeviceManagerCallbacks = 3100;
	k_iClientMusicCallbacks = 3200;
	k_iClientRemoteClientManagerCallbacks = 3300;
	k_iClientUGCCallbacks = 3400;
	k_iSteamStreamClientCallbacks = 3500;
	k_IClientProductBuilderCallbacks = 3600;
	k_iClientShortcutsCallbacks = 3700;
	k_iClientRemoteControlManagerCallbacks = 3800;
	k_iSteamAppListCallbacks = 3900;
	k_iSteamMusicCallbacks = 4000;
	k_iSteamMusicRemoteCallbacks = 4100;
	k_iClientVRCallbacks = 4200;
	k_iClientReservedCallbacks = 4300;
	k_iSteamReservedCallbacks = 4400;
	k_iSteamHTMLSurfaceCallbacks = 4500;
	k_iClientVideoCallbacks = 4600;
	k_cchMaxFriendsGroupName = 64;
	k_cFriendsGroupLimit = 100;
	k_cEnumerateFollowersMax = 50;
	k_cchPersonaNameMax = 128;
	k_cwchPersonaNameMax = 32;
	k_cubChatMetadataMax = 8192;
	k_cchMaxRichPresenceKeys = 20;
	k_cchMaxRichPresenceKeyLength = 64;
	k_cchMaxRichPresenceValueLength = 256;
	k_unServerFlagNone = $00;
	k_unServerFlagActive = $01;
	k_unServerFlagSecure = $02;
	k_unServerFlagDedicated = $04;
	k_unServerFlagLinux = $08;
	k_unServerFlagPassworded = $10;
	k_unServerFlagPrivate = $20;
	k_unFavoriteFlagNone = $00;
	k_unFavoriteFlagFavorite = $01;
	k_unFavoriteFlagHistory = $02;
	k_unMaxCloudFileChunkSize = 100 * 1024 * 1024;
	k_cchPublishedDocumentTitleMax = 128 + 1;
	k_cchPublishedDocumentDescriptionMax = 8000;
	k_cchPublishedDocumentChangeDescriptionMax = 8000;
	k_unEnumeratePublishedFilesMaxResults = 50;
	k_cchTagListMax = 1024 + 1;
	k_cchFilenameMax = 260;
	k_cchPublishedFileURLMax = 256;
	k_nScreenshotMaxTaggedUsers = 32;
	k_nScreenshotMaxTaggedPublishedFiles = 32;
	k_cubUFSTagTypeMax = 255;
	k_cubUFSTagValueMax = 255;
	k_ScreenshotThumbWidth = 200;
	kNumUGCResultsPerPage = 50;
	k_cchStatNameMax = 128;
	k_cchLeaderboardNameMax = 128;
	k_cLeaderboardDetailsMax = 64;
	k_cbMaxGameServerGameDir = 32;
	k_cbMaxGameServerMapName = 32;
	k_cbMaxGameServerGameDescription = 64;
	k_cbMaxGameServerName = 64;
	k_cbMaxGameServerTags = 128;
	k_cbMaxGameServerGameData = 2048;
	k_unSteamAccountIDMask = -1;
	k_unSteamAccountInstanceMask = $000FFFFF;
	k_unSteamUserDesktopInstance = 1;
	k_unSteamUserConsoleInstance = 2;
	k_unSteamUserWebInstance = 4;
	k_cchGameExtraInfoMax = 64;
	k_nSteamEncryptedAppTicketSymmetricKeyLen = 32;
	k_cubSaltSize = 8;
	k_GIDNil = $ffffffffffffffff;
	k_TxnIDNil = k_GIDNil;
	k_TxnIDUnknown = 0;
	k_uPackageIdFreeSub:Cardinal = $0;
	k_uPackageIdInvalid:Cardinal = $FFFFFFFF;
	k_ulAssetClassIdInvalid = $0;
	k_uPhysicalItemIdInvalid:Cardinal = $0;
	k_uCellIDInvalid:Cardinal = $FFFFFFFF;
	k_uPartnerIdInvalid:Cardinal = 0;
	MASTERSERVERUPDATERPORT_USEGAMESOCKETSHARE = -1;
	INVALID_HTTPREQUEST_HANDLE:Byte = 0;
	k_nMaxLobbyKeyLength:Byte = 255;
	k_SteamMusicNameMaxLength = 255;
	k_SteamMusicPNGMaxLength = 65535;
	QUERY_PORT_NOT_INITIALIZED = $FFFF;
	QUERY_PORT_ERROR = $FFFE;
	STEAM_RIGHT_TRIGGER_MASK = $0000000000000001;
	STEAM_LEFT_TRIGGER_MASK = $0000000000000002;
	STEAM_RIGHT_BUMPER_MASK = $0000000000000004;
	STEAM_LEFT_BUMPER_MASK = $0000000000000008;
	STEAM_BUTTON_0_MASK = $0000000000000010;
	STEAM_BUTTON_1_MASK = $0000000000000020;
	STEAM_BUTTON_2_MASK = $0000000000000040;
	STEAM_BUTTON_3_MASK = $0000000000000080;
	STEAM_TOUCH_0_MASK = $0000000000000100;
	STEAM_TOUCH_1_MASK = $0000000000000200;
	STEAM_TOUCH_2_MASK = $0000000000000400;
	STEAM_TOUCH_3_MASK = $0000000000000800;
	STEAM_BUTTON_MENU_MASK = $0000000000001000;
	STEAM_BUTTON_STEAM_MASK = $0000000000002000;
	STEAM_BUTTON_ESCAPE_MASK = $0000000000004000;
	STEAM_BUTTON_BACK_LEFT_MASK = $0000000000008000;
	STEAM_BUTTON_BACK_RIGHT_MASK = $0000000000010000;
	STEAM_BUTTON_LEFTPAD_CLICKED_MASK = $0000000000020000;
	STEAM_BUTTON_RIGHTPAD_CLICKED_MASK = $0000000000040000;
	STEAM_LEFTPAD_FINGERDOWN_MASK = $0000000000080000;
	STEAM_RIGHTPAD_FINGERDOWN_MASK = $0000000000100000;
	MAX_STEAM_CONTROLLERS:Byte = 8;

Type
	SteamRegisterActivationCodeResult = (
		SteamRegisterActivationCodeResultOK = 0,
		SteamRegisterActivationCodeResultFail = 1,
		SteamRegisterActivationCodeResultAlreadyRegistered = 2,
		SteamRegisterActivationCodeResultTimeout = 3,
		SteamRegisterActivationCodeAlreadyOwned = 4
	);
	SteamControllerPad = (
		SteamSteamControllerPad_Left = 2,
		SteamSteamControllerPad_Right = 2
	);
	SteamFriendRelationship = (
		SteamFriendRelationshipNone = 0,
		SteamFriendRelationshipBlocked = 1,
		SteamFriendRelationshipRequestRecipient = 2,
		SteamFriendRelationshipFriend = 3,
		SteamFriendRelationshipRequestInitiator = 4,
		SteamFriendRelationshipIgnored = 5,
		SteamFriendRelationshipIgnoredFriend = 6,
		SteamFriendRelationshipSuggested = 7,	// keep this updated
		SteamFriendRelationshipMax = 8
	);
	SteamPersonaState = (
		SteamPersonaStateOffline = 0,	// friend is not currently logged on
		SteamPersonaStateOnline = 1,	// friend is logged on
		SteamPersonaStateBusy = 2,	// user is on, but busy
		SteamPersonaStateAway = 3,	// auto-away feature
		SteamPersonaStateSnooze = 4,	// auto-away for a long time
		SteamPersonaStateLookingToTrade = 5,	// Online, trading
		SteamPersonaStateLookingToPlay = 6,	// Online, wanting to play
		SteamPersonaStateMax = 4
	);
	SteamFriendFlags = (
		SteamFriendFlagNone = $00,
		SteamFriendFlagBlocked = $01,
		SteamFriendFlagFriendshipRequested = $02,
		SteamFriendFlagImmediate = $04,	// "regular" friend
		SteamFriendFlagClanMember = $08,
		SteamFriendFlagOnGameServer = $10,	// k_EFriendFlagFriendOfFriend	= 0x40, // not currently used
		SteamFriendFlagRequestingFriendship = $80,
		SteamFriendFlagRequestingInfo = $100,
		SteamFriendFlagIgnored = $200,
		SteamFriendFlagIgnoredFriend = $400,
		SteamFriendFlagSuggested = $800,
		SteamFriendFlagAll = $FFFF
	);
	SteamUserRestriction = (
		k_nUserRestrictionNone = 0,	// no known chat/content restriction
		k_nUserRestrictionUnknown = 1,	// we don't know yet (user offline)
		k_nUserRestrictionAnyChat = 2,	// user is not allowed to (or can't) send/recv any chat
		k_nUserRestrictionVoiceChat = 4,	// user is not allowed to (or can't) send/recv voice chat
		k_nUserRestrictionGroupChat = 8,	// user is not allowed to (or can't) send/recv group chat
		k_nUserRestrictionRating = 16,	// user is too young according to rating in current region
		k_nUserRestrictionGameInvites = 32,	// user cannot send or recv game invites (e.g. mobile)
		k_nUserRestrictionTrading = 64	// user cannot participate in trading (console, mobile)
	);
	SteamOverlayToStoreFlag = (
		SteamOverlayToStoreFlag_None = 0,
		SteamOverlayToStoreFlag_AddToCart = 1,
		SteamOverlayToStoreFlag_AddToCartAndShow = 2
	);
	SteamPersonaChange = (
		SteamPersonaChangeName = $0001,
		SteamPersonaChangeStatus = $0002,
		SteamPersonaChangeComeOnline = $0004,
		SteamPersonaChangeGoneOffline = $0008,
		SteamPersonaChangeGamePlayed = $0010,
		SteamPersonaChangeGameServer = $0020,
		SteamPersonaChangeAvatar = $0040,
		SteamPersonaChangeJoinedSource = $0080,
		SteamPersonaChangeLeftSource = $0100,
		SteamPersonaChangeRelationshipChanged = $0200,
		SteamPersonaChangeNameFirstSet = $0400,
		SteamPersonaChangeFacebookInfo = $0800,
		SteamPersonaChangeNickname = $1000,
		SteamPersonaChangeSteamLevel = $2000
	);
	SteamGCResults = (
		SteamGCResultOK = 0,
		SteamGCResultNoMessage = 1,	// There is no message in the queue
		SteamGCResultBufferTooSmall = 2,	// The buffer is too small for the requested message
		SteamGCResultNotLoggedOn = 3,	// The client is not logged onto Steam
		SteamGCResultInvalidMessage = 4	// Something was wrong with the message being sent with SendMessage
	);
	SteamHTMLMouseButton = (
		eHTMLMouseButton_Left = 0,
		eHTMLMouseButton_Right = 1,
		eHTMLMouseButton_Middle = 2
	);
	SteamMouseCursor = (
		dc_user = 0,
		dc_none = 11,
		dc_arrow = 11,
		dc_ibeam = 11,
		dc_hourglass = 11,
		dc_waitarrow = 11,
		dc_crosshair = 11,
		dc_up = 11,
		dc_sizenw = 11,
		dc_sizese = 11,
		dc_sizene = 11,
		dc_sizesw = 11,
		dc_sizew = 11,
		dc_sizee = 11,
		dc_sizen = 11,
		dc_sizes = 11,
		dc_sizewe = 11,
		dc_sizens = 11,
		dc_sizeall = 11,
		dc_no = 11,
		dc_hand = 11,
		dc_blank = 11,	// don't show any custom cursor, just use your default
		dc_middle_pan = 11,
		dc_north_pan = 11,
		dc_north_east_pan = 11,
		dc_east_pan = 11,
		dc_south_east_pan = 11,
		dc_south_pan = 11,
		dc_south_west_pan = 11,
		dc_west_pan = 11,
		dc_north_west_pan = 11,
		dc_alias = 11,
		dc_cell = 11,
		dc_colresize = 11,
		dc_copycur = 11,
		dc_verticaltext = 11,
		dc_rowresize = 11,
		dc_zoomin = 11,
		dc_zoomout = 11,
		dc_help = 11,
		dc_custom = 11,
		dc_last = 11	// custom cursors start from this value and up
	);
	SteamHTMLKeyModifiers = (
		eHTMLKeyModifier_None = 0,
		eHTMLKeyModifier_AltDown = 1 Shl 0,
		eHTMLKeyModifier_CrtlDown = 1 Shl 1,
		eHTMLKeyModifier_ShiftDown = 1 Shl 2
	);
	SteamLobbyType = (
		SteamLobbyTypePrivate = 0,	// only way to join the lobby is to invite to someone else
		SteamLobbyTypeFriendsOnly = 1,	// shows for friends or invitees, but not in lobby list
		SteamLobbyTypePublic = 2,	// visible for friends and in lobby list
		SteamLobbyTypeInvisible = 3	// a user can be in only one regular lobby, and up to two invisible lobbies
	);
	SteamLobbyComparison = (
		SteamLobbyComparisonEqualToOrLessThan = -2,
		SteamLobbyComparisonLessThan = -1,
		SteamLobbyComparisonEqual = 0,
		SteamLobbyComparisonGreaterThan = 1,
		SteamLobbyComparisonEqualToOrGreaterThan = 2,
		SteamLobbyComparisonNotEqual = 3
	);
	SteamLobbyDistanceFilter = (
		SteamLobbyDistanceFilterClose = 15,	// only lobbies in the same immediate region will be returned
		SteamLobbyDistanceFilterDefault = 15,	// only lobbies in the same region or near by regions
		SteamLobbyDistanceFilterFar = 15,	// for games that don't have many latency requirements, will return lobbies about half-way around the globe
		SteamLobbyDistanceFilterWorldwide = 15	// no filtering, will match lobbies as far as India to NY (not recommended, expect multiple seconds of latency between the clients)
	);
	SteamChatMemberStateChange = (
		SteamChatMemberStateChangeEntered = $0001,	// This user has joined or is joining the chat room
		SteamChatMemberStateChangeLeft = $0002,	// This user has left or is leaving the chat room
		SteamChatMemberStateChangeDisconnected = $0004,	// User disconnected without leaving the chat first
		SteamChatMemberStateChangeKicked = $0008,	// User kicked
		SteamChatMemberStateChangeBanned = $0010	// User kicked and banned
	);
	SteamAudioPlayback_Status = (
		AudioPlayback_Undefined = 0,
		AudioPlayback_Playing = 1,
		AudioPlayback_Paused = 2,
		AudioPlayback_Idle = 3
	);
	SteamP2PSessionError = (
		SteamP2PSessionErrorNone = 0,
		SteamP2PSessionErrorNotRunningApp = 1,	// target is not running the same game
		SteamP2PSessionErrorNoRightsToApp = 2,	// local user doesn't own the app that is running
		SteamP2PSessionErrorDestinationNotLoggedIn = 3,	// target user isn't connected to Steam
		SteamP2PSessionErrorTimeout = 4,	// make sure that UDP ports 3478, 4379, and 4380 are open in an outbound direction
		SteamP2PSessionErrorMax = 5
	);
	SteamP2PSend = (
		SteamP2PSendUnreliable = 0,	// This is only really useful for kinds of data that should never buffer up, i.e. voice payload packets
		SteamP2PSendUnreliableNoDelay = 1,	// Does fragmentation/re-assembly of messages under the hood, as well as a sliding window for efficient sends of large chunks of data.
		SteamP2PSendReliable = 2,	// do a normal k_EP2PSendReliable to force all the buffered data to be sent.
		SteamP2PSendReliableWithBuffering = 3
	);
	SteamSNetSocketState = (
		SteamSNetSocketStateInvalid = 0,	// communication is valid
		SteamSNetSocketStateConnected = 1,	// states while establishing a connection
		SteamSNetSocketStateInitiated = 10,	// p2p connections
		SteamSNetSocketStateLocalCandidatesFound = 11,	// we've found our local IP info
		SteamSNetSocketStateReceivedRemoteCandidates = 12,	// direct connections
		SteamSNetSocketStateChallengeHandshake = 15,	// failure states
		SteamSNetSocketStateDisconnecting = 21,	// the API shut it down, and we're in the process of telling the other end
		SteamSNetSocketStateLocalDisconnect = 22,	// the API shut it down, and we've completed shutdown
		SteamSNetSocketStateTimeoutDuringConnect = 23,	// we timed out while trying to creating the connection
		SteamSNetSocketStateRemoteEndDisconnected = 24,	// the remote end has disconnected from us
		SteamSNetSocketStateConnectionBroken = 25	// connection has been broken; either the other end has disappeared or our local network connection has broke
	);
	SteamSNetSocketConnectionType = (
		SteamSNetSocketConnectionTypeNotConnected = 0,
		SteamSNetSocketConnectionTypeUDP = 1,
		SteamSNetSocketConnectionTypeUDPRelay = 2
	);
	SteamResolveConflict = (
		SteamResolveConflictKeepClient = 1,	// The local version of each file will be used to overwrite the server version
		SteamResolveConflictKeepServer = 2	// The server version of each file will be used to overwrite the local version
	);
	SteamRemoteStoragePlatform = (
		SteamRemoteStoragePlatformNone = 0,
		SteamRemoteStoragePlatformWindows = (1 Shl 0),
		SteamRemoteStoragePlatformOSX = (1 Shl 1),
		SteamRemoteStoragePlatformPS3 = (1 Shl 2),
		SteamRemoteStoragePlatformLinux = (1 Shl 3),
		SteamRemoteStoragePlatformReserved2 = (1 Shl 4),
		SteamRemoteStoragePlatformAll = -1
	);
	SteamRemoteStoragePublishedFileVisibility = (
		SteamRemoteStoragePublishedFileVisibilityPublic = 0,
		SteamRemoteStoragePublishedFileVisibilityFriendsOnly = 1,
		SteamRemoteStoragePublishedFileVisibilityPrivate = 2
	);
	SteamWorkshopFileType = (
		SteamWorkshopFileTypeFirst = 0,
		SteamWorkshopFileTypeCommunity = 0,
		SteamWorkshopFileTypeMicrotransaction = 1,
		SteamWorkshopFileTypeCollection = 2,
		SteamWorkshopFileTypeArt = 3,
		SteamWorkshopFileTypeVideo = 4,
		SteamWorkshopFileTypeScreenshot = 5,
		SteamWorkshopFileTypeGame = 6,
		SteamWorkshopFileTypeSoftware = 7,
		SteamWorkshopFileTypeConcept = 8,
		SteamWorkshopFileTypeWebGuide = 9,
		SteamWorkshopFileTypeIntegratedGuide = 10,
		SteamWorkshopFileTypeMerch = 11,
		SteamWorkshopFileTypeControllerBinding = 12,
		SteamWorkshopFileTypeSteamworksAccessInvite = 13,
		SteamWorkshopFileTypeSteamVideo = 14,	// Update k_EWorkshopFileTypeMax if you add values.
		SteamWorkshopFileTypeMax = 15
	);
	SteamWorkshopVote = (
		SteamWorkshopVoteUnvoted = 0,
		SteamWorkshopVoteFor = 1,
		SteamWorkshopVoteAgainst = 2
	);
	SteamWorkshopFileAction = (
		SteamWorkshopFileActionPlayed = 0,
		SteamWorkshopFileActionCompleted = 1
	);
	SteamWorkshopEnumerationType = (
		SteamWorkshopEnumerationTypeRankedByVote = 0,
		SteamWorkshopEnumerationTypeRecent = 1,
		SteamWorkshopEnumerationTypeTrending = 2,
		SteamWorkshopEnumerationTypeFavoritesOfFriends = 3,
		SteamWorkshopEnumerationTypeVotedByFriends = 4,
		SteamWorkshopEnumerationTypeContentByFriends = 5,
		SteamWorkshopEnumerationTypeRecentFromFollowedUsers = 6
	);
	SteamWorkshopVideoProvider = (
		SteamWorkshopVideoProviderNone = 0,
		SteamWorkshopVideoProviderYoutube = 1
	);
	SteamUGCReadAction = (
		SteamUGCRead_ContinueReadingUntilFinished = 0,	// When you are done seeking around the file, make a final call with k_EUGCRead_Close to close it.
		SteamUGCRead_ContinueReading = 1,	// To read the file from Steam again you will need to call UGCDownload again.
		SteamUGCRead_Close = 2
	);
	SteamUGCMatchingUGCType = (
		SteamUGCMatchingUGCType_Items = 0,	// both mtx items and ready-to-use items
		SteamUGCMatchingUGCType_Items_Mtx = 1,
		SteamUGCMatchingUGCType_Items_ReadyToUse = 2,
		SteamUGCMatchingUGCType_Collections = 3,
		SteamUGCMatchingUGCType_Artwork = 4,
		SteamUGCMatchingUGCType_Videos = 5,
		SteamUGCMatchingUGCType_Screenshots = 6,
		SteamUGCMatchingUGCType_AllGuides = 7,	// both web guides and integrated guides
		SteamUGCMatchingUGCType_WebGuides = 8,
		SteamUGCMatchingUGCType_IntegratedGuides = 9,
		SteamUGCMatchingUGCType_UsableInGame = 10,	// ready-to-use items and integrated guides
		SteamUGCMatchingUGCType_ControllerBindings = 11
	);
	SteamUserUGCList = (
		SteamUserUGCList_Published = 32,
		SteamUserUGCList_VotedOn = 32,
		SteamUserUGCList_VotedUp = 32,
		SteamUserUGCList_VotedDown = 32,
		SteamUserUGCList_WillVoteLater = 32,
		SteamUserUGCList_Favorited = 32,
		SteamUserUGCList_Subscribed = 32,
		SteamUserUGCList_UsedOrPlayed = 32,
		SteamUserUGCList_Followed = 32
	);
	SteamUserUGCListSortOrder = (
		SteamUserUGCListSortOrder_CreationOrderDesc = 33,
		SteamUserUGCListSortOrder_CreationOrderAsc = 33,
		SteamUserUGCListSortOrder_TitleAsc = 33,
		SteamUserUGCListSortOrder_LastUpdatedDesc = 33,
		SteamUserUGCListSortOrder_SubscriptionDateDesc = 33,
		SteamUserUGCListSortOrder_VoteScoreDesc = 33,
		SteamUserUGCListSortOrder_ForModeration = 33
	);
	SteamUGCQuery = (
		SteamUGCQuery_RankedByVote = 0,
		SteamUGCQuery_RankedByPublicationDate = 1,
		SteamUGCQuery_AcceptedForGameRankedByAcceptanceDate = 2,
		SteamUGCQuery_RankedByTrend = 3,
		SteamUGCQuery_FavoritedByFriendsRankedByPublicationDate = 4,
		SteamUGCQuery_CreatedByFriendsRankedByPublicationDate = 5,
		SteamUGCQuery_RankedByNumTimesReported = 6,
		SteamUGCQuery_CreatedByFollowedUsersRankedByPublicationDate = 7,
		SteamUGCQuery_NotYetRated = 8,
		SteamUGCQuery_RankedByTotalVotesAsc = 9,
		SteamUGCQuery_RankedByVotesUp = 10,
		SteamUGCQuery_RankedByTextSearch = 11
	);
	SteamItemUpdateStatus = (
		SteamItemUpdateStatusInvalid = 0,	// The item update handle was invalid, job might be finished, listen too SubmitItemUpdateResult_t
		SteamItemUpdateStatusPreparingConfig = 1,	// The item update is processing configuration data
		SteamItemUpdateStatusPreparingContent = 2,	// The item update is reading and processing content files
		SteamItemUpdateStatusUploadingContent = 3,	// The item update is uploading content changes to Steam
		SteamItemUpdateStatusUploadingPreviewFile = 4,	// The item update is uploading new preview file image
		SteamItemUpdateStatusCommittingChanges = 5  // The item update is committing all changes
	);
	SteamLeaderboardDataRequest = (
		SteamLeaderboardDataRequestGlobal = 0,
		SteamLeaderboardDataRequestGlobalAroundUser = 1,
		SteamLeaderboardDataRequestFriends = 2,
		SteamLeaderboardDataRequestUsers = 3
	);
	SteamLeaderboardSortMethod = (
		SteamLeaderboardSortMethodNone = 0,
		SteamLeaderboardSortMethodAscending = 1,	// top-score is lowest number
		SteamLeaderboardSortMethodDescending = 2	// top-score is highest number
	);
	SteamLeaderboardDisplayType = (
		SteamLeaderboardDisplayTypeNone = 0,
		SteamLeaderboardDisplayTypeNumeric = 1,	// simple numerical score
		SteamLeaderboardDisplayTypeTimeSeconds = 2,	// the score represents a time, in seconds
		SteamLeaderboardDisplayTypeTimeMilliSeconds = 3	// the score represents a time, in milliseconds
	);
	SteamLeaderboardUploadScoreMethod = (
		SteamLeaderboardUploadScoreMethodNone = 0,
		SteamLeaderboardUploadScoreMethodKeepBest = 1,	// Leaderboard will keep user's best score
		SteamLeaderboardUploadScoreMethodForceUpdate = 2	// Leaderboard will always replace score with specified
	);
	SteamAPICallFailure = (
		SteamSteamAPICallFailureNone = -1,	// no failure
		SteamSteamAPICallFailureSteamGone = 0,	// the local Steam process has gone away
		SteamSteamAPICallFailureNetworkFailure = 1,	// SteamServersConnected_t will be sent when the client is able to talk to the Steam servers again
		SteamSteamAPICallFailureInvalidHandle = 2,	// the SteamAPICall_t handle passed in no longer exists
		SteamSteamAPICallFailureMismatchedCallback = 3	// GetAPICallResult() was called with the wrong callback type for this API call
	);
	SteamGamepadTextInputMode = (
		SteamGamepadTextInputModeNormal = 0,
		SteamGamepadTextInputModePassword = 1
	);
	SteamGamepadTextInputLineMode = (
		SteamGamepadTextInputLineModeSingleLine = 0,
		SteamGamepadTextInputLineModeMultipleLines = 1
	);
	SteamCheckFileSignature = (
		SteamCheckFileSignatureInvalidSignature = 0,
		SteamCheckFileSignatureValidSignature = 1,
		SteamCheckFileSignatureFileNotFound = 2,
		SteamCheckFileSignatureNoSignaturesFoundForThisApp = 3,
		SteamCheckFileSignatureNoSignaturesFoundForThisFile = 4
	);
	SteamMatchMakingServerResponse = (
		eServerResponded = 0,
		eServerFailedToRespond = 44,
		eNoServersListedOnMasterServer = 44	// for the Internet query type, returned in response callback if no servers of this type match
	);
	SteamResult = (
		SteamResultOK = 1,	// success
		SteamResultFail = 2,	// generic failure
		SteamResultNoConnection = 3,	// = 4,				// OBSOLETE - removed
		SteamResultInvalidPassword = 5,	// password/ticket is invalid
		SteamResultLoggedInElsewhere = 6,	// same user logged in elsewhere
		SteamResultInvalidProtocolVer = 7,	// protocol version is incorrect
		SteamResultInvalidParam = 8,	// a parameter is incorrect
		SteamResultFileNotFound = 9,	// file was not found
		SteamResultBusy = 10,	// called method busy - action not taken
		SteamResultInvalidState = 11,	// called object was in an invalid state
		SteamResultInvalidName = 12,	// name is invalid
		SteamResultInvalidEmail = 13,	// email is invalid
		SteamResultDuplicateName = 14,	// name is not unique
		SteamResultAccessDenied = 15,	// access is denied
		SteamResultTimeout = 16,	// operation timed out
		SteamResultBanned = 17,	// VAC2 banned
		SteamResultAccountNotFound = 18,	// account not found
		SteamResultInvalidSteamID = 19,	// steamID is invalid
		SteamResultServiceUnavailable = 20,	// The requested service is currently unavailable
		SteamResultNotLoggedOn = 21,	// The user is not logged on
		SteamResultPending = 22,	// Request is pending (may be in process, or waiting on third party)
		SteamResultEncryptionFailure = 23,	// Encryption or Decryption failed
		SteamResultInsufficientPrivilege = 24,	// Insufficient privilege
		SteamResultLimitExceeded = 25,	// Too much of a good thing
		SteamResultRevoked = 26,	// Access has been revoked (used for revoked guest passes)
		SteamResultExpired = 27,	// License/Guest pass the user is trying to access is expired
		SteamResultAlreadyRedeemed = 28,	// Guest pass has already been redeemed by account, cannot be acked again
		SteamResultDuplicateRequest = 29,	// The request is a duplicate and the action has already occurred in the past, ignored this time
		SteamResultAlreadyOwned = 30,	// All the games in this guest pass redemption request are already owned by the user
		SteamResultIPNotFound = 31,	// IP address not found
		SteamResultPersistFailed = 32,	// failed to write change to the data store
		SteamResultLockingFailed = 33,	// failed to acquire access lock for this operation
		SteamResultLogonSessionReplaced = 34,
		SteamResultConnectFailed = 35,
		SteamResultHandshakeFailed = 36,
		SteamResultIOFailure = 37,
		SteamResultRemoteDisconnect = 38,
		SteamResultShoppingCartNotFound = 39,	// failed to find the shopping cart requested
		SteamResultBlocked = 40,	// a user didn't allow it
		SteamResultIgnored = 41,	// target is ignoring sender
		SteamResultNoMatch = 42,	// nothing matching the request found
		SteamResultAccountDisabled = 43,
		SteamResultServiceReadOnly = 44,	// this service is not accepting content changes right now
		SteamResultAccountNotFeatured = 45,	// account doesn't have value, so this feature isn't available
		SteamResultAdministratorOK = 46,	// allowed to take this action, but only because requester is admin
		SteamResultContentVersion = 47,	// A Version mismatch in content transmitted within the Steam protocol.
		SteamResultTryAnotherCM = 48,	// The current CM can't service the user making a request, user should try another.
		SteamResultPasswordRequiredToKickSession = 49,	// You are already logged in elsewhere, this cached credential login has failed.
		SteamResultAlreadyLoggedInElsewhere = 50,	// You are already logged in elsewhere, you must wait
		SteamResultSuspended = 51,	// Long running operation (content download) suspended/paused
		SteamResultCancelled = 52,	// Operation canceled (typically by user: content download)
		SteamResultDataCorruption = 53,	// Operation canceled because data is ill formed or unrecoverable
		SteamResultDiskFull = 54,	// Operation canceled - not enough disk space.
		SteamResultRemoteCallFailed = 55,	// an remote call or IPC call failed
		SteamResultPasswordUnset = 56,	// Password could not be verified as it's unset server side
		SteamResultExternalAccountUnlinked = 57,	// External account (PSN, Facebook...) is not linked to a Steam account
		SteamResultPSNTicketInvalid = 58,	// PSN ticket was invalid
		SteamResultExternalAccountAlreadyLinked = 59,	// External account (PSN, Facebook...) is already linked to some other account, must explicitly request to replace/delete the link first
		SteamResultRemoteFileConflict = 60,	// The sync cannot resume due to a conflict between the local and remote files
		SteamResultIllegalPassword = 61,	// The requested new password is not legal
		SteamResultSameAsPreviousValue = 62,	// new value is the same as the old one ( secret question and answer )
		SteamResultAccountLogonDenied = 63,	// account login denied due to 2nd factor authentication failure
		SteamResultCannotUseOldPassword = 64,	// The requested new password is not legal
		SteamResultInvalidLoginAuthCode = 65,	// account login denied due to auth code invalid
		SteamResultAccountLogonDeniedNoMail = 66,	// account login denied due to 2nd factor auth failure - and no mail has been sent
		SteamResultHardwareNotCapableOfIPT = 67,
		SteamResultIPTInitError = 68,
		SteamResultParentalControlRestricted = 69,	// operation failed due to parental control restrictions for current user
		SteamResultFacebookQueryError = 70,	// Facebook query returned an error
		SteamResultExpiredLoginAuthCode = 71,	// account login denied due to auth code expired
		SteamResultIPLoginRestrictionFailed = 72,
		SteamResultAccountLockedDown = 73,
		SteamResultAccountLogonDeniedVerifiedEmailRequired = 74,
		SteamResultNoMatchingURL = 75,
		SteamResultBadResponse = 76,	// parse failure, missing field, etc.
		SteamResultRequirePasswordReEntry = 77,	// The user cannot complete the action until they re-enter their password
		SteamResultValueOutOfRange = 78,	// the value entered is outside the acceptable range
		SteamResultUnexpectedError = 79,	// something happened that we didn't expect to ever happen
		SteamResultDisabled = 80,	// The requested service has been configured to be unavailable
		SteamResultInvalidCEGSubmission = 81,	// The set of files submitted to the CEG server are not valid !
		SteamResultRestrictedDevice = 82,	// The device being used is not allowed to perform this action
		SteamResultRegionLocked = 83,	// The action could not be complete because it is region restricted
		SteamResultRateLimitExceeded = 84,	// Temporary rate limit exceeded, try again later, different from k_EResultLimitExceeded which may be permanent
		SteamResultAccountLoginDeniedNeedTwoFactor = 85,	// Need two-factor code to login
		SteamResultItemDeleted = 86,	// The thing we're trying to access has been deleted
		SteamResultAccountLoginDeniedThrottle = 87,	// login attempt failed, try to throttle response to possible attacker
		SteamResultTwoFactorCodeMismatch = 88,	// two factor code mismatch
		SteamResultTwoFactorActivationCodeMismatch = 89	// activation code for two-factor didn't match
	);
	SteamVoiceResult = (
		SteamVoiceResultOK = 0,
		SteamVoiceResultNotInitialized = 1,
		SteamVoiceResultNotRecording = 2,
		SteamVoiceResultNoData = 3,
		SteamVoiceResultBufferTooSmall = 4,
		SteamVoiceResultDataCorrupted = 5,
		SteamVoiceResultRestricted = 6,
		SteamVoiceResultUnsupportedCodec = 7
	);
	SteamDenyReason = (
		SteamDenyInvalid = 0,
		SteamDenyInvalidVersion = 1,
		SteamDenyGeneric = 2,
		SteamDenyNotLoggedOn = 3,
		SteamDenyNoLicense = 4,
		SteamDenyCheater = 5,
		SteamDenyLoggedInElseWhere = 6,
		SteamDenyUnknownText = 7,
		SteamDenyIncompatibleAnticheat = 8,
		SteamDenyMemoryCorruption = 9,
		SteamDenyIncompatibleSoftware = 10,
		SteamDenySteamConnectionLost = 11,
		SteamDenySteamConnectionError = 12,
		SteamDenySteamResponseTimedOut = 13,
		SteamDenySteamValidationStalled = 14,
		SteamDenySteamOwnerLeftGuestUser = 15
	);
	SteamBeginAuthSessionResult = (
		SteamBeginAuthSessionResultOK = 0,	// Ticket is valid for this game and this steamID.
		SteamBeginAuthSessionResultInvalidTicket = 1,	// Ticket is not valid.
		SteamBeginAuthSessionResultDuplicateRequest = 2,	// A ticket has already been submitted for this steamID
		SteamBeginAuthSessionResultInvalidVersion = 3,	// Ticket is from an incompatible interface version
		SteamBeginAuthSessionResultGameMismatch = 4,	// Ticket is not for this game
		SteamBeginAuthSessionResultExpiredTicket = 5	// Ticket has expired
	);
	SteamAuthSessionResponse = (
		SteamAuthSessionResponseOK = 0,	// Steam has verified the user is online, the ticket is valid and ticket has not been reused.
		SteamAuthSessionResponseUserNotConnectedToSteam = 1,	// The user in question is not connected to steam
		SteamAuthSessionResponseNoLicenseOrExpired = 2,	// The license has expired.
		SteamAuthSessionResponseVACBanned = 3,	// The user is VAC banned for this game.
		SteamAuthSessionResponseLoggedInElseWhere = 4,	// The user account has logged in elsewhere and the session containing the game instance has been disconnected.
		SteamAuthSessionResponseVACCheckTimedOut = 5,	// VAC has been unable to perform anti-cheat checks on this user
		SteamAuthSessionResponseAuthTicketCanceled = 6,	// The ticket has been canceled by the issuer
		SteamAuthSessionResponseAuthTicketInvalidAlreadyUsed = 7,	// This ticket has already been used, it is not valid.
		SteamAuthSessionResponseAuthTicketInvalid = 8,	// This ticket is not from a user instance currently connected to steam.
		SteamAuthSessionResponsePublisherIssuedBan = 9	// The user is banned for this game. The ban came via the web api and not VAC
	);
	SteamUserHasLicenseForAppResult = (
		SteamUserHasLicenseResultHasLicense = 0,	// User has a license for specified app
		SteamUserHasLicenseResultDoesNotHaveLicense = 1,	// User does not have a license for the specified app
		SteamUserHasLicenseResultNoAuth = 2	// User has not been authenticated
	);
	SteamAccountType = (
		SteamAccountTypeInvalid = 0,
		SteamAccountTypeIndividual = 1,	// single user account
		SteamAccountTypeMultiseat = 2,	// multiseat (e.g. cybercafe) account
		SteamAccountTypeGameServer = 3,	// game server account
		SteamAccountTypeAnonGameServer = 4,	// anonymous game server account
		SteamAccountTypePending = 5,	// pending
		SteamAccountTypeContentServer = 6,	// content server
		SteamAccountTypeClan = 7,
		SteamAccountTypeChat = 8,
		SteamAccountTypeConsoleUser = 9,	// Fake SteamID for local PSN account on PS3 or Live account on 360, etc.
		SteamAccountTypeAnonUser = 10,	// Max of 16 items in this field
		SteamAccountTypeMax = 51
	);
	SteamAppReleaseState = (
		SteamAppReleaseState_Unknown = 0,	// unknown, required appinfo or license info is missing
		SteamAppReleaseState_Unavailable = 1,	// even if user 'just' owns it, can see game at all
		SteamAppReleaseState_Prerelease = 2,	// can be purchased and is visible in games list, nothing else. Common appInfo section released
		SteamAppReleaseState_PreloadOnly = 3,	// owners can preload app, not play it. AppInfo fully released.
		SteamAppReleaseState_Released = 4	// owners can download and play app.
	);
	SteamAppOwnershipFlags = (
		SteamAppOwnershipFlags_None = $0000,	// unknown
		SteamAppOwnershipFlags_OwnsLicense = $0001,	// owns license for this game
		SteamAppOwnershipFlags_FreeLicense = $0002,	// not paid for game
		SteamAppOwnershipFlags_RegionRestricted = $0004,	// owns app, but not allowed to play in current region
		SteamAppOwnershipFlags_LowViolence = $0008,	// only low violence version
		SteamAppOwnershipFlags_InvalidPlatform = $0010,	// app not supported on current platform
		SteamAppOwnershipFlags_SharedLicense = $0020,	// license was granted by authorized local device
		SteamAppOwnershipFlags_FreeWeekend = $0040,	// owned by a free weekend licenses
		SteamAppOwnershipFlags_RetailLicense = $0080,	// has a retail license for game, (CD-Key etc)
		SteamAppOwnershipFlags_LicenseLocked = $0100,	// shared license is locked (in use) by other user
		SteamAppOwnershipFlags_LicensePending = $0200,	// owns app, but transaction is still pending. Can't install or play
		SteamAppOwnershipFlags_LicenseExpired = $0400,	// doesn't own app anymore since license expired
		SteamAppOwnershipFlags_LicensePermanent = $0800,	// permanent license, not borrowed, or guest or freeweekend etc
		SteamAppOwnershipFlags_LicenseRecurring = $1000,	// Recurring license, user is charged periodically
		SteamAppOwnershipFlags_LicenseCanceled = $2000	// Mark as canceled, but might be still active if recurring
	);
	SteamAppType = (
		SteamAppType_Invalid = $000,	// unknown / invalid
		SteamAppType_Game = $001,	// playable game, default type
		SteamAppType_Application = $002,	// software application
		SteamAppType_Tool = $004,	// SDKs, editors & dedicated servers
		SteamAppType_Demo = $008,	// game demo
		SteamAppType_Media_DEPRECATED = $010,	// legacy - was used for game trailers, which are now just videos on the web
		SteamAppType_DLC = $020,	// down loadable content
		SteamAppType_Guide = $040,	// game guide, PDF etc
		SteamAppType_Driver = $080,	// hardware driver updater (ATI, Razor etc)
		SteamAppType_Config = $100,	// hidden app used to config Steam features (backpack, sales, etc)
		SteamAppType_Film = $200,	// A Movie (feature film)
		SteamAppType_TVSeries = $400,	// A TV or other video series which will have episodes and perhaps seasons
		SteamAppType_Video = $800,	// A video component of either a Film or TVSeries (may be the feature, an episode, preview, making-of, etc)
		SteamAppType_Plugin = $1000,	// Plug-in types for other Apps
		SteamAppType_Music = $2000,	// Music files
		SteamAppType_Shortcut = $40000000,	// just a shortcut, client side only
		SteamAppType_DepotOnly = -2147483647	// placeholder since depots and apps share the same namespace
	);
	SteamUserStatType = (
		SteamSteamUserStatTypeINVALID = 0,
		SteamSteamUserStatTypeINT = 1,
		SteamSteamUserStatTypeFLOAT = 2,	// Read as FLOAT, set with count / session length
		SteamSteamUserStatTypeAVGRATE = 3,
		SteamSteamUserStatTypeACHIEVEMENTS = 4,
		SteamSteamUserStatTypeGROUPACHIEVEMENTS = 5,	// max, for sanity checks
		SteamSteamUserStatTypeMAX = 55
	);
	SteamChatEntryType = (
		SteamChatEntryTypeInvalid = 0,
		SteamChatEntryTypeChatMsg = 1,	// Normal text message from another user
		SteamChatEntryTypeTyping = 2,	// Another user is typing (not used in multi-user chat)
		SteamChatEntryTypeInviteGame = 3,	// Invite from other user into that users current game
		SteamChatEntryTypeEmote = 4,	// = 5,	// lobby game is starting (dead - listen for LobbyGameCreated_t callback instead)
		SteamChatEntryTypeLeftConversation = 6,	// Above are previous FriendMsgType entries, now merged into more generic chat entry types
		SteamChatEntryTypeEntered = 7,	// user has entered the conversation (used in multi-user chat and group chat)
		SteamChatEntryTypeWasKicked = 8,	// user was kicked (data: 64-bit steamid of actor performing the kick)
		SteamChatEntryTypeWasBanned = 9,	// user was banned (data: 64-bit steamid of actor performing the ban)
		SteamChatEntryTypeDisconnected = 10,	// user disconnected
		SteamChatEntryTypeHistoricalChat = 11,	// a chat message from user's chat history or offilne message
		SteamChatEntryTypeReserved1 = 12,
		SteamChatEntryTypeReserved2 = 13
	);
	SteamChatRoomEnterResponse = (
		SteamChatRoomEnterResponseSuccess = 1,	// Success
		SteamChatRoomEnterResponseDoesntExist = 2,	// Chat doesn't exist (probably closed)
		SteamChatRoomEnterResponseNotAllowed = 3,	// General Denied - You don't have the permissions needed to join the chat
		SteamChatRoomEnterResponseFull = 4,	// Chat room has reached its maximum size
		SteamChatRoomEnterResponseError = 5,	// Unexpected Error
		SteamChatRoomEnterResponseBanned = 6,	// You are banned from this chat room and may not join
		SteamChatRoomEnterResponseLimited = 7,	// Joining this chat is not allowed because you are a limited user (no value on account)
		SteamChatRoomEnterResponseClanDisabled = 8,	// Attempt to join a clan chat when the clan is locked or disabled
		SteamChatRoomEnterResponseCommunityBan = 9,	// Attempt to join a chat when the user has a community lock on their account
		SteamChatRoomEnterResponseMemberBlockedYou = 10,	// Join failed - some member in the chat has blocked you from joining
		SteamChatRoomEnterResponseYouBlockedMember = 11	// k_EChatRoomEnterResponseRankOutOfRange = 14, //  No longer used
	);
	ChatSteamIDInstanceFlags = (
		SteamChatAccountInstanceMask = $00000FFF,	// top 8 bits are flags
		SteamChatInstanceFlagClan = ( $00000FFF + 1 ) Shr 1,	// top bit
		SteamChatInstanceFlagLobby = ( $00000FFF + 1 ) Shr 2,	// next one down, etc
		SteamChatInstanceFlagMMSLobby = ( $00000FFF + 1 ) Shr 3	// Max of 8 flags
	);
	SteamMarketingMessageFlags = (
		SteamMarketingMessageFlagsNone = 0,
		SteamMarketingMessageFlagsHighPriority = 1 Shl 0,
		SteamMarketingMessageFlagsPlatformWindows = 1 Shl 1,
		SteamMarketingMessageFlagsPlatformMac = 1 Shl 2,
		SteamMarketingMessageFlagsPlatformLinux = 1 Shl 3,	// flags
		SteamMarketingMessageFlagsPlatformRestrictions = $FFFF
	);
	SteamNotificationPosition = (
		SteamPositionTopLeft = 0,
		SteamPositionTopRight = 1,
		SteamPositionBottomLeft = 2,
		SteamPositionBottomRight = 3
	);
	SteamHTTPMethod = (
		SteamHTTPMethodInvalid = 0,
		SteamHTTPMethodGET = 61,
		SteamHTTPMethodHEAD = 61,
		SteamHTTPMethodPOST = 61,
		SteamHTTPMethodPUT = 61,
		SteamHTTPMethodDELETE = 61,
		SteamHTTPMethodOPTIONS = 61	// k_EHTTPMethodCONNECT
	);
	SteamHTTPStatusCode = (
		SteamHTTPStatusCodeInvalid = 0,	// Informational codes
		SteamHTTPStatusCode100Continue = 100,
		SteamHTTPStatusCode101SwitchingProtocols = 101,	// Success codes
		SteamHTTPStatusCode200OK = 200,
		SteamHTTPStatusCode201Created = 201,
		SteamHTTPStatusCode202Accepted = 202,
		SteamHTTPStatusCode203NonAuthoritative = 203,
		SteamHTTPStatusCode204NoContent = 204,
		SteamHTTPStatusCode205ResetContent = 205,
		SteamHTTPStatusCode206PartialContent = 206,	// Redirection codes
		SteamHTTPStatusCode300MultipleChoices = 300,
		SteamHTTPStatusCode301MovedPermanently = 301,
		SteamHTTPStatusCode302Found = 302,
		SteamHTTPStatusCode303SeeOther = 303,
		SteamHTTPStatusCode304NotModified = 304,
		SteamHTTPStatusCode305UseProxy = 305,	// =				306, (used in old HTTP spec, now unused in 1.1)
		SteamHTTPStatusCode307TemporaryRedirect = 307,	// Error codes
		SteamHTTPStatusCode400BadRequest = 400,
		SteamHTTPStatusCode401Unauthorized = 401,
		SteamHTTPStatusCode402PaymentRequired = 402,	// This is reserved for future HTTP specs, not really supported by clients
		SteamHTTPStatusCode403Forbidden = 403,
		SteamHTTPStatusCode404NotFound = 404,
		SteamHTTPStatusCode405MethodNotAllowed = 405,
		SteamHTTPStatusCode406NotAcceptable = 406,
		SteamHTTPStatusCode407ProxyAuthRequired = 407,
		SteamHTTPStatusCode408RequestTimeout = 408,
		SteamHTTPStatusCode409Conflict = 409,
		SteamHTTPStatusCode410Gone = 410,
		SteamHTTPStatusCode411LengthRequired = 411,
		SteamHTTPStatusCode412PreconditionFailed = 412,
		SteamHTTPStatusCode413RequestEntityTooLarge = 413,
		SteamHTTPStatusCode414RequestURITooLong = 414,
		SteamHTTPStatusCode415UnsupportedMediaType = 415,
		SteamHTTPStatusCode416RequestedRangeNotSatisfiable = 416,
		SteamHTTPStatusCode417ExpectationFailed = 417,
		SteamHTTPStatusCode429TooManyRequests = 429,	// Server error codes
		SteamHTTPStatusCode500InternalServerError = 500,
		SteamHTTPStatusCode501NotImplemented = 501,
		SteamHTTPStatusCode502BadGateway = 502,
		SteamHTTPStatusCode503ServiceUnavailable = 503,
		SteamHTTPStatusCode504GatewayTimeout = 504,
		SteamHTTPStatusCode505HTTPVersionNotSupported = 505
	);
	SteamUniverse = (
		SteamUniverseInvalid = 0,
		SteamUniversePublic = 1,
		SteamUniverseBeta = 2,
		SteamUniverseInternal = 3,
		SteamUniverseDev = 4,	// k_EUniverseRC = 5,				// no such universe anymore
		SteamUniverseMax = 63
	);
	SteamServerMode = (
		eServerModeInvalid = 0,	// DO NOT USE
		eServerModeNoAuthentication = 1,	// Don't authenticate user logins and don't list on the server list
		eServerModeAuthentication = 2,	// Authenticate users, list on the server list, don't run VAC on clients that connect
		eServerModeAuthenticationAndSecure = 3	// Authenticate users, list on the server list and VAC protect clients
	);

Type
	SteamFriendGameInfo = Packed Record
			gameID:SteamGameID;
			GameIP:Cardinal;
			GamePort:Word;
			QueryPort:Word;
			steamIDLobby:SteamID;
	End;

	SteamFriendSessionStateInfo = Packed Record
			uiOnlineSessionInstances:Cardinal;
			uiPublishedToFriendsSessionInstance:Byte;
	End;

	SteamP2PSessionState = Packed Record
			bConnectionActive:Byte;	// true if we've got an active open connection
			bConnecting:Byte;	// true if we're currently trying to establish a connection
			eP2PSessionError:Byte;	// last error recorded (see enum above)
			bUsingRelay:Byte;	// true if it's going through a relay server (TURN)
			nBytesQueuedForSend:Integer;
			nPacketsQueuedForSend:Integer;
			nRemoteIP:Cardinal;	// potential IP:Port of remote host. Could be TURN server.
			nRemotePort:Word;	// Only exists for compatibility with older authentication api's
	End;

	SteamParamStringArray = Packed Record
			ppStrings:Pointer;
			nNumStrings:Integer;
	End;

	SteamUGCDetails = Packed Record
			nPublishedFileId:SteamPublishedFileId;
			eResult:SteamResult;	// The result of the operation.
			eFileType:SteamWorkshopFileType;	// Type of the file
			nCreatorAppID:SteamAppId;	// ID of the app that created this file.
			nConsumerAppID:SteamAppId;	// ID of the app that will consume this file.
			rgchTitle:Array[0..Pred(k_cchPublishedDocumentTitleMax)] Of AnsiChar;	// title of document
			rgchDescription:Array[0..Pred(k_cchPublishedDocumentDescriptionMax)] Of AnsiChar;	// description of document
			ulSteamIDOwner:UInt64;	// Steam ID of the user who created this content.
			rtimeCreated:Cardinal;	// time when the published file was created
			rtimeUpdated:Cardinal;	// time when the published file was last updated
			rtimeAddedToUserList:Cardinal;	// time when the user added the published file to their list (not always applicable)
			eVisibility:SteamRemoteStoragePublishedFileVisibility;	// visibility
			bBanned:Boolean;	// whether the file was banned
			bAcceptedForUse:Boolean;	// developer has specifically flagged this item as accepted in the Workshop
			bTagsTruncated:Boolean;	// whether the list of tags was too long to be returned in the provided buffer
			rgchTags:Array[0..Pred(k_cchTagListMax)] Of AnsiChar;	// file/url information
			hFile:SteamUGCHandle;	// The handle of the primary file
			hPreviewFile:SteamUGCHandle;	// The handle of the preview file
			pchFileName:Array[0..Pred(k_cchFilenameMax)] Of AnsiChar;	// The cloud filename of the primary file
			nFileSize:Integer;	// Size of the primary file
			nPreviewFileSize:Integer;	// Size of the preview file
			rgchURL:Array[0..Pred(k_cchPublishedFileURLMax)] Of AnsiChar;	// voting information
			VotesUp:Cardinal;	// number of votes up
			VotesDown:Cardinal;	// number of votes down
			flScore:Single;	// calculated score
			NumChildren:Cardinal;	// if m_eFileType == k_EWorkshopFileTypeCollection, then this number will be the number of children contained within the collection
	End;

	SteamCallbackMsg = Packed Record
			hSteamUser:Integer;
			iCallback:Integer;
			ptrParam:Pointer;
			cubParam:Integer;
	End;

	SteamLeaderboardEntry = Packed Record
			steamIDUser:SteamID;	// user with the entry - use SteamFriends()->GetFriendPersonaName() & SteamFriends()->GetFriendAvatar() to get more info
			nGlobalRank:Integer;	// [1..N], where N is the number of users with an entry in the leaderboard
			nScore:Integer;	// score as set in the leaderboard
			cDetails:Integer;	// number of int32 details available for this entry
			hUGC:SteamUGCHandle;	// handle for UGC attached to the entry
	End;

	SteamMatchMakingKeyValuePair = Packed Record
			szKey:Array[0..Pred(256)] Of AnsiChar;
			szValue:Array[0..Pred(256)] Of AnsiChar;
	End;

	SteamControllerState = Packed Record
			unPacketNum:Cardinal;	// bit flags for each of the buttons
			ulButtons:UInt64;	// Left pad coordinates
			sLeftPadX:SmallInt;
			sLeftPadY:SmallInt;	// Right pad coordinates
			sRightPadX:SmallInt;
			sRightPadY:SmallInt;
	End;

  SteamGameType = (
    SteamGameIDTypeApp = 0,
    SteamGameIDTypeGameMod = 1,
    SteamGameIDTypeShortcut = 2,
    SteamGameIDTypeP2P = 3
  );

  SteamAPIWarningMessageHook = Procedure (nSeverity:Integer; pchDebugText:PAnsiChar); Cdecl; 
  SteamAPI_PostAPIResultInProcess = Procedure(callHandle:SteamAPICall; pUnknown:Pointer; unCallbackSize:Cardinal; iCallbackNum:Integer); Cdecl; 
  SteamAPI_CheckCallbackRegistered = Procedure(iCallbackNum:Integer); Cdecl; 

// steam_api.h
Var
	SteamAPI_Shutdown: Procedure(); CDecl;
	SteamAPI_IsSteamRunning: Function():Boolean; CDecl;
	SteamAPI_RestartAppIfNecessary: Function(unOwnAppID:SteamAppId):Boolean; CDecl;
	SteamAPI_WriteMiniDump: Procedure(uStructuredExceptionCode:Cardinal; pvExceptionInfo:Pointer; uBuildID:Cardinal); CDecl;
	SteamAPI_SetMiniDumpComment: Procedure(pchMsg:PAnsiChar); CDecl;
	SteamClient: Function():Pointer; CDecl;
	SteamAPI_InitSafe: Function():Boolean; CDecl;
	SteamAPI_RunCallbacks: Procedure(); CDecl;
	SteamAPI_RegisterCallback: Procedure(pCallback:Pointer; iCallback:Integer); CDecl;
	SteamAPI_UnregisterCallback: Procedure(pCallback:Pointer); CDecl;
	SteamAPI_RegisterCallResult: Procedure(pCallback:Pointer; hAPICall:UInt64); CDecl;
	SteamAPI_UnregisterCallResult: Procedure(pCallback:Pointer; hAPICall:UInt64); CDecl;
	Steam_RunCallbacks: Procedure(hSteamPipe:SteamPipeHandle; bGameServerCallbacks:Boolean); CDecl;
	Steam_RegisterInterfaceFuncs: Procedure(hModule:Pointer); CDecl;
	Steam_GetHSteamUserCurrent: Function():Integer; CDecl;
	SteamAPI_GetSteamInstallPath: Function():Integer; CDecl;
	SteamAPI_GetHSteamPipe: Function():Integer; CDecl;
	SteamAPI_SetTryCatchCallbacks: Procedure(bTryCatchCallbacks:Boolean); CDecl;
	SteamAPI_GetHSteamUser: Function():Integer; CDecl;
	SteamAPI_UseBreakpadCrashHandler: Procedure(pchVersion:PAnsiChar; pchDate:PAnsiChar; pchTime:PAnsiChar; bFullMemoryDumps:Boolean; pvContext:Pointer; m_pfnPreMinidumpCallback:Pointer); CDecl;
	SteamUser: Function():Pointer; CDecl;
	SteamFriends: Function():Pointer; CDecl;
	SteamUtils: Function():Pointer; CDecl;
	SteamMatchmaking: Function():Pointer; CDecl;
	SteamUserStats: Function():Pointer; CDecl;
	SteamApps: Function():Pointer; CDecl;
	SteamNetworking: Function():Pointer; CDecl;
	SteamMatchmakingServers: Function():Pointer; CDecl;
	SteamRemoteStorage: Function():Pointer; CDecl;
	SteamScreenshots: Function():Pointer; CDecl;
	SteamHTTP: Function():Pointer; CDecl;
	SteamUnifiedMessages: Function():Pointer; CDecl;
	SteamController: Function():Pointer; CDecl;
	SteamUGC: Function():Pointer; CDecl;
	SteamAppList: Function():Pointer; CDecl;
	SteamMusic: Function():Pointer; CDecl;
	SteamMusicRemote: Function():Pointer; CDecl;

// steam_gameserver.h
Var
	SteamGameServer_InitSafe: Function(unIP:Cardinal; usSteamPort:Word; usGamePort:Word; usQueryPort:Word; eServerMode:SteamServerMode; pchVersionString:PAnsiChar):Boolean; CDecl;
	SteamGameServer_Shutdown: Procedure(); CDecl;
	SteamGameServer_RunCallbacks: Procedure(); CDecl;
	SteamGameServer_BSecure: Function():Boolean; CDecl;
	SteamGameServer_GetSteamID: Function():UInt64; CDecl;
	SteamGameServer_GetHSteamPipe: Function():Integer; CDecl;
	SteamGameServer_GetHSteamUser: Function():Integer; CDecl;
	SteamClientGameServer: Function():Pointer; CDecl;
	SteamGameServer: Function():Pointer; CDecl;
	SteamGameServerUtils: Function():Pointer; CDecl;
	SteamGameServerNetworking: Function():Pointer; CDecl;
	SteamGameServerStats: Function():Pointer; CDecl;
	SteamGameServerHTTP: Function():Pointer; CDecl;

// steamencryptedappticket.h
Var
	BDecryptTicket: Function(rgubTicketEncrypted:PByte; cubTicketEncrypted:Cardinal; rgubTicketDecrypted:PByte; Var pcubTicketDecrypted:Cardinal; rgubKey:PByte; cubKey:Integer):Boolean; CDecl;
	BIsTicketForApp: Function(rgubTicketDecrypted:PByte; cubTicketDecrypted:Cardinal; nAppID:SteamAppId):Boolean; CDecl;
	GetTicketIssueTime: Function(rgubTicketDecrypted:PByte; cubTicketDecrypted:Cardinal):Cardinal; CDecl;
	GetTicketSteamID: Procedure(rgubTicketDecrypted:PByte; cubTicketDecrypted:Cardinal; Var psteamID:SteamID); CDecl;
	GetTicketAppID: Function(rgubTicketDecrypted:PByte; cubTicketDecrypted:Cardinal):Cardinal; CDecl;
	BUserOwnsAppInTicket: Function(rgubTicketDecrypted:PByte; cubTicketDecrypted:Cardinal; nAppID:SteamAppId):Boolean; CDecl;
	BUserIsVacBanned: Function(rgubTicketDecrypted:PByte; cubTicketDecrypted:Cardinal):Boolean; CDecl;
	GetUserVariableData: Function(rgubTicketDecrypted:PByte; cubTicketDecrypted:Cardinal; Var pcubUserData:Cardinal):Pointer; CDecl;

// SteamAppList
Var
	ISteamAppList_GetNumInstalledApps: Function():Cardinal; CDecl;
	ISteamAppList_GetInstalledApps: Function(pvecAppID:PSteamAppId; unMaxAppIDs:Cardinal):Cardinal; CDecl;
	ISteamAppList_GetAppName: Function(nAppID:SteamAppId; pchName:Pointer; cchNameMax:Integer):Integer; CDecl;
	ISteamAppList_GetAppInstallDir: Function(nAppID:SteamAppId; pchDirectory:Pointer; cchNameMax:Integer):Integer; CDecl;
	ISteamAppList_GetAppBuildId: Function(nAppID:SteamAppId):Integer; CDecl;

// SteamApps
Var
	ISteamApps_BIsSubscribed: Function():Boolean; CDecl;
	ISteamApps_BIsLowViolence: Function():Boolean; CDecl;
	ISteamApps_BIsCybercafe: Function():Boolean; CDecl;
	ISteamApps_BIsVACBanned: Function():Boolean; CDecl;
	ISteamApps_GetCurrentGameLanguage: Function():PAnsiChar; CDecl;
	ISteamApps_GetAvailableGameLanguages: Function():PAnsiChar; CDecl;
	ISteamApps_BIsSubscribedApp: Function(appID:SteamAppId):Boolean; CDecl;
	ISteamApps_BIsDlcInstalled: Function(appID:SteamAppId):Boolean; CDecl;
	ISteamApps_GetEarliestPurchaseUnixTime: Function(nAppID:SteamAppId):Cardinal; CDecl;
	ISteamApps_BIsSubscribedFromFreeWeekend: Function():Boolean; CDecl;
	ISteamApps_GetDLCCount: Function():Integer; CDecl;
	ISteamApps_BGetDLCDataByIndex: Function(iDLC:Integer; Var pAppID:SteamAppId; Var pbAvailable:Boolean; pchName:Pointer; cchNameBufferSize:Integer):Boolean; CDecl;
	ISteamApps_InstallDLC: Procedure(nAppID:SteamAppId); CDecl;
	ISteamApps_UninstallDLC: Procedure(nAppID:SteamAppId); CDecl;
	ISteamApps_RequestAppProofOfPurchaseKey: Procedure(nAppID:SteamAppId); CDecl;
	ISteamApps_GetCurrentBetaName: Function(pchName:Pointer; cchNameBufferSize:Integer):Boolean; CDecl;
	ISteamApps_MarkContentCorrupt: Function(bMissingFilesOnly:Boolean):Boolean; CDecl;
	ISteamApps_GetInstalledDepots: Function(appID:SteamAppId; pvecDepots:PSteamDepotId; cMaxDepots:Cardinal):Cardinal; CDecl;
	ISteamApps_GetAppInstallDir: Function(appID:SteamAppId; pchFolder:Pointer; cchFolderBufferSize:Cardinal):Cardinal; CDecl;
	ISteamApps_BIsAppInstalled: Function(appID:SteamAppId):Boolean; CDecl;
	ISteamApps_GetAppOwner: Function():UInt64; CDecl;
	ISteamApps_GetLaunchQueryParam: Function(pchKey:PAnsiChar):PAnsiChar; CDecl;

// SteamClient
Var
	ISteamClient_CreateSteamPipe: Function():Integer; CDecl;
	ISteamClient_BReleaseSteamPipe: Function(hSteamPipe:SteamPipeHandle):Boolean; CDecl;
	ISteamClient_ConnectToGlobalUser: Function(hSteamPipe:SteamPipeHandle):Integer; CDecl;
	ISteamClient_CreateLocalUser: Function(Var phSteamPipe:SteamPipeHandle; eAccountType:SteamAccountType):Integer; CDecl;
	ISteamClient_ReleaseUser: Procedure(hSteamPipe:SteamPipeHandle; hUser:SteamUserHandle); CDecl;
	ISteamClient_GetISteamUser: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamGameServer: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_SetLocalIPBinding: Procedure(unIP:Cardinal; usPort:Word); CDecl;
	ISteamClient_GetISteamFriends: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamUtils: Function(hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamMatchmaking: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamMatchmakingServers: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamGenericInterface: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamUserStats: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamGameServerStats: Function(hSteamuser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamApps: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamNetworking: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamRemoteStorage: Function(hSteamuser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamScreenshots: Function(hSteamuser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_RunFrame: Procedure(); CDecl;
	ISteamClient_GetIPCCallCount: Function():Cardinal; CDecl;
	ISteamClient_SetWarningMessageHook: Procedure(pFunction:SteamAPIWarningMessageHook); CDecl;
	ISteamClient_BShutdownIfAllPipesClosed: Function():Boolean; CDecl;
	ISteamClient_GetISteamHTTP: Function(hSteamuser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamUnifiedMessages: Function(hSteamuser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamController: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamUGC: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamAppList: Function(hSteamUser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamMusic: Function(hSteamuser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamMusicRemote: Function(hSteamuser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_GetISteamHTMLSurface: Function(hSteamuser:SteamUserHandle; hSteamPipe:SteamPipeHandle; pchVersion:PAnsiChar):Pointer; CDecl;
	ISteamClient_Set_SteamAPI_CPostAPIResultInProcess: Procedure(func:SteamAPI_PostAPIResultInProcess); CDecl;
	ISteamClient_Remove_SteamAPI_CPostAPIResultInProcess: Procedure(func:SteamAPI_PostAPIResultInProcess); CDecl;
	ISteamClient_Set_SteamAPI_CCheckCallbackRegisteredInProcess: Procedure(func:SteamAPI_CheckCallbackRegistered); CDecl;

// SteamController
Var
	ISteamController_Init: Function(pchAbsolutePathToControllerConfigVDF:PAnsiChar):Boolean; CDecl;
	ISteamController_Shutdown: Function():Boolean; CDecl;
	ISteamController_RunFrame: Procedure(); CDecl;
	ISteamController_GetControllerState: Function(unControllerIndex:Cardinal; Var pState:SteamControllerState):Boolean; CDecl;
	ISteamController_TriggerHapticPulse: Procedure(unControllerIndex:Cardinal; eTargetPad:SteamControllerPad; usDurationMicroSec:Word); CDecl;
	ISteamController_SetOverrideMode: Procedure(pchMode:PAnsiChar); CDecl;

// SteamFriends
Var
	ISteamFriends_GetPersonaName: Function():PAnsiChar; CDecl;
	ISteamFriends_SetPersonaName: Function(pchPersonaName:PAnsiChar):UInt64; CDecl;
	ISteamFriends_GetPersonaState: Function():SteamPersonaState; CDecl;
	ISteamFriends_GetFriendCount: Function(iFriendFlags:SteamFriendFlags):Integer; CDecl;
	ISteamFriends_GetFriendByIndex: Function(iFriend:Integer; iFriendFlags:SteamFriendFlags):UInt64; CDecl;
	ISteamFriends_GetFriendRelationship: Function(steamIDFriend:SteamID):SteamFriendRelationship; CDecl;
	ISteamFriends_GetFriendPersonaState: Function(steamIDFriend:SteamID):SteamPersonaState; CDecl;
	ISteamFriends_GetFriendPersonaName: Function(steamIDFriend:SteamID):PAnsiChar; CDecl;
	ISteamFriends_GetFriendGamePlayed: Function(steamIDFriend:SteamID; Var pFriendGameInfo:SteamFriendGameInfo):Boolean; CDecl;
	ISteamFriends_GetFriendPersonaNameHistory: Function(steamIDFriend:SteamID; iPersonaName:Integer):PAnsiChar; CDecl;
	ISteamFriends_GetPlayerNickname: Function(steamIDPlayer:SteamID):PAnsiChar; CDecl;
	ISteamFriends_HasFriend: Function(steamIDFriend:SteamID; iFriendFlags:SteamFriendFlags):Boolean; CDecl;
	ISteamFriends_GetClanCount: Function():Integer; CDecl;
	ISteamFriends_GetClanByIndex: Function(iClan:Integer):UInt64; CDecl;
	ISteamFriends_GetClanName: Function(steamIDClan:SteamID):PAnsiChar; CDecl;
	ISteamFriends_GetClanTag: Function(steamIDClan:SteamID):PAnsiChar; CDecl;
	ISteamFriends_GetClanActivityCounts: Function(steamIDClan:SteamID; Var pnOnline:Integer; Var pnInGame:Integer; Var pnChatting:Integer):Boolean; CDecl;
	ISteamFriends_DownloadClanActivityCounts: Function(psteamIDClans:PSteamID; cClansToRequest:Integer):UInt64; CDecl;
	ISteamFriends_GetFriendCountFromSource: Function(steamIDSource:SteamID):Integer; CDecl;
	ISteamFriends_GetFriendFromSourceByIndex: Function(steamIDSource:SteamID; iFriend:Integer):UInt64; CDecl;
	ISteamFriends_IsUserInSource: Function(steamIDUser:SteamID; steamIDSource:SteamID):Boolean; CDecl;
	ISteamFriends_SetInGameVoiceSpeaking: Procedure(steamIDUser:SteamID; bSpeaking:Boolean); CDecl;
	ISteamFriends_ActivateGameOverlay: Procedure(pchDialog:PAnsiChar); CDecl;
	ISteamFriends_ActivateGameOverlayToUser: Procedure(pchDialog:PAnsiChar; steamID:SteamID); CDecl;
	ISteamFriends_ActivateGameOverlayToWebPage: Procedure(pchURL:PAnsiChar); CDecl;
	ISteamFriends_ActivateGameOverlayToStore: Procedure(nAppID:SteamAppId; eFlag:SteamOverlayToStoreFlag); CDecl;
	ISteamFriends_SetPlayedWith: Procedure(steamIDUserPlayedWith:SteamID); CDecl;
	ISteamFriends_ActivateGameOverlayInviteDialog: Procedure(steamIDLobby:SteamID); CDecl;
	ISteamFriends_GetSmallFriendAvatar: Function(steamIDFriend:SteamID):Integer; CDecl;
	ISteamFriends_GetMediumFriendAvatar: Function(steamIDFriend:SteamID):Integer; CDecl;
	ISteamFriends_GetLargeFriendAvatar: Function(steamIDFriend:SteamID):Integer; CDecl;
	ISteamFriends_RequestUserInformation: Function(steamIDUser:SteamID; bRequireNameOnly:Boolean):Boolean; CDecl;
	ISteamFriends_RequestClanOfficerList: Function(steamIDClan:SteamID):UInt64; CDecl;
	ISteamFriends_GetClanOwner: Function(steamIDClan:SteamID):UInt64; CDecl;
	ISteamFriends_GetClanOfficerCount: Function(steamIDClan:SteamID):Integer; CDecl;
	ISteamFriends_GetClanOfficerByIndex: Function(steamIDClan:SteamID; iOfficer:Integer):UInt64; CDecl;
	ISteamFriends_GetUserRestrictions: Function():Cardinal; CDecl;
	ISteamFriends_SetRichPresence: Function(pchKey:PAnsiChar; pchValue:PAnsiChar):Boolean; CDecl;
	ISteamFriends_ClearRichPresence: Procedure(); CDecl;
	ISteamFriends_GetFriendRichPresence: Function(steamIDFriend:SteamID; pchKey:PAnsiChar):PAnsiChar; CDecl;
	ISteamFriends_GetFriendRichPresenceKeyCount: Function(steamIDFriend:SteamID):Integer; CDecl;
	ISteamFriends_GetFriendRichPresenceKeyByIndex: Function(steamIDFriend:SteamID; iKey:Integer):PAnsiChar; CDecl;
	ISteamFriends_RequestFriendRichPresence: Procedure(steamIDFriend:SteamID); CDecl;
	ISteamFriends_InviteUserToGame: Function(steamIDFriend:SteamID; pchConnectString:PAnsiChar):Boolean; CDecl;
	ISteamFriends_GetCoplayFriendCount: Function():Integer; CDecl;
	ISteamFriends_GetCoplayFriend: Function(iCoplayFriend:Integer):UInt64; CDecl;
	ISteamFriends_GetFriendCoplayTime: Function(steamIDFriend:SteamID):Integer; CDecl;
	ISteamFriends_GetFriendCoplayGame: Function(steamIDFriend:SteamID):Cardinal; CDecl;
	ISteamFriends_JoinClanChatRoom: Function(steamIDClan:SteamID):UInt64; CDecl;
	ISteamFriends_LeaveClanChatRoom: Function(steamIDClan:SteamID):Boolean; CDecl;
	ISteamFriends_GetClanChatMemberCount: Function(steamIDClan:SteamID):Integer; CDecl;
	ISteamFriends_GetChatMemberByIndex: Function(steamIDClan:SteamID; iUser:Integer):UInt64; CDecl;
	ISteamFriends_SendClanChatMessage: Function(steamIDClanChat:SteamID; pchText:PAnsiChar):Boolean; CDecl;
	ISteamFriends_GetClanChatMessage: Function(steamIDClanChat:SteamID; iMessage:Integer; prgchText:Pointer; cchTextMax:Integer; Var peChatEntryType:SteamChatEntryType; Var psteamidChatter:SteamID):Integer; CDecl;
	ISteamFriends_IsClanChatAdmin: Function(steamIDClanChat:SteamID; steamIDUser:SteamID):Boolean; CDecl;
	ISteamFriends_IsClanChatWindowOpenInSteam: Function(steamIDClanChat:SteamID):Boolean; CDecl;
	ISteamFriends_OpenClanChatWindowInSteam: Function(steamIDClanChat:SteamID):Boolean; CDecl;
	ISteamFriends_CloseClanChatWindowInSteam: Function(steamIDClanChat:SteamID):Boolean; CDecl;
	ISteamFriends_SetListenForFriendsMessages: Function(bInterceptEnabled:Boolean):Boolean; CDecl;
	ISteamFriends_ReplyToFriendMessage: Function(steamIDFriend:SteamID; pchMsgToSend:PAnsiChar):Boolean; CDecl;
	ISteamFriends_GetFriendMessage: Function(steamIDFriend:SteamID; iMessageID:Integer; pvData:Pointer; cubData:Integer; Var peChatEntryType:SteamChatEntryType):Integer; CDecl;
	ISteamFriends_GetFollowerCount: Function(steamID:SteamID):UInt64; CDecl;
	ISteamFriends_IsFollowing: Function(steamID:SteamID):UInt64; CDecl;
	ISteamFriends_EnumerateFollowingList: Function(unStartIndex:Cardinal):UInt64; CDecl;

// SteamGameServer
Var
	ISteamGameServer_InitGameServer: Function(unIP:Cardinal; usGamePort:Word; usQueryPort:Word; unFlags:Cardinal; nGameAppId:SteamAppId; pchVersionString:PAnsiChar):Boolean; CDecl;
	ISteamGameServer_SetProduct: Procedure(pszProduct:PAnsiChar); CDecl;
	ISteamGameServer_SetGameDescription: Procedure(pszGameDescription:PAnsiChar); CDecl;
	ISteamGameServer_SetModDir: Procedure(pszModDir:PAnsiChar); CDecl;
	ISteamGameServer_SetDedicatedServer: Procedure(bDedicated:Boolean); CDecl;
	ISteamGameServer_LogOn: Procedure(pszToken:PAnsiChar); CDecl;
	ISteamGameServer_LogOnAnonymous: Procedure(); CDecl;
	ISteamGameServer_LogOff: Procedure(); CDecl;
	ISteamGameServer_BLoggedOn: Function():Boolean; CDecl;
	ISteamGameServer_BSecure: Function():Boolean; CDecl;
	ISteamGameServer_GetSteamID: Function():UInt64; CDecl;
	ISteamGameServer_WasRestartRequested: Function():Boolean; CDecl;
	ISteamGameServer_SetMaxPlayerCount: Procedure(cPlayersMax:Integer); CDecl;
	ISteamGameServer_SetBotPlayerCount: Procedure(cBotplayers:Integer); CDecl;
	ISteamGameServer_SetServerName: Procedure(pszServerName:PAnsiChar); CDecl;
	ISteamGameServer_SetMapName: Procedure(pszMapName:PAnsiChar); CDecl;
	ISteamGameServer_SetPasswordProtected: Procedure(bPasswordProtected:Boolean); CDecl;
	ISteamGameServer_SetSpectatorPort: Procedure(unSpectatorPort:Word); CDecl;
	ISteamGameServer_SetSpectatorServerName: Procedure(pszSpectatorServerName:PAnsiChar); CDecl;
	ISteamGameServer_ClearAllKeyValues: Procedure(); CDecl;
	ISteamGameServer_SetKeyValue: Procedure(pKey:PAnsiChar; pValue:PAnsiChar); CDecl;
	ISteamGameServer_SetGameTags: Procedure(pchGameTags:PAnsiChar); CDecl;
	ISteamGameServer_SetGameData: Procedure(pchGameData:PAnsiChar); CDecl;
	ISteamGameServer_SetRegion: Procedure(pszRegion:PAnsiChar); CDecl;
	ISteamGameServer_SendUserConnectAndAuthenticate: Function(unIPClient:Cardinal; pvAuthBlob:PByte; cubAuthBlobSize:Cardinal; Var pSteamIDUser:SteamID):Boolean; CDecl;
	ISteamGameServer_CreateUnauthenticatedUserConnection: Function():UInt64; CDecl;
	ISteamGameServer_SendUserDisconnect: Procedure(steamIDUser:SteamID); CDecl;
	ISteamGameServer_BUpdateUserData: Function(steamIDUser:SteamID; pchPlayerName:PAnsiChar; uScore:Cardinal):Boolean; CDecl;
	ISteamGameServer_GetAuthSessionTicket: Function(pTicket:PByte; cbMaxTicket:Integer; Var pcbTicket:Cardinal):Cardinal; CDecl;
	ISteamGameServer_BeginAuthSession: Function(pAuthTicket:PByte; cbAuthTicket:Integer; steamID:SteamID):SteamBeginAuthSessionResult; CDecl;
	ISteamGameServer_EndAuthSession: Procedure(steamID:SteamID); CDecl;
	ISteamGameServer_CancelAuthTicket: Procedure(hAuthTicket:SteamAuthTicketHandle); CDecl;
	ISteamGameServer_UserHasLicenseForApp: Function(steamID:SteamID; appID:SteamAppId):SteamUserHasLicenseForAppResult; CDecl;
	ISteamGameServer_RequestUserGroupStatus: Function(steamIDUser:SteamID; steamIDGroup:SteamID):Boolean; CDecl;
	ISteamGameServer_GetGameplayStats: Procedure(); CDecl;
	ISteamGameServer_GetServerReputation: Function():UInt64; CDecl;
	ISteamGameServer_GetPublicIP: Function():Cardinal; CDecl;
	ISteamGameServer_HandleIncomingPacket: Function(pData:PByte; cbData:Integer; srcIP:Cardinal; srcPort:Word):Boolean; CDecl;
	ISteamGameServer_GetNextOutgoingPacket: Function(pOut:PByte; cbMaxOut:Integer; Var pNetAdr:Cardinal; Var pPort:Word):Integer; CDecl;
	ISteamGameServer_EnableHeartbeats: Procedure(bActive:Boolean); CDecl;
	ISteamGameServer_SetHeartbeatInterval: Procedure(iHeartbeatInterval:Integer); CDecl;
	ISteamGameServer_ForceHeartbeat: Procedure(); CDecl;
	ISteamGameServer_AssociateWithClan: Function(steamIDClan:SteamID):UInt64; CDecl;
	ISteamGameServer_ComputeNewPlayerCompatibility: Function(steamIDNewPlayer:SteamID):UInt64; CDecl;

// SteamGameServerHTTP
Var
	ISteamGameServerHTTP_CreateHTTPRequest: Function(eHTTPRequestMethod:SteamHTTPMethod; pchAbsoluteURL:PAnsiChar):SteamHTTPRequestHandle; CDecl;
	ISteamGameServerHTTP_SetHTTPRequestContextValue: Function(hRequest:SteamHTTPRequestHandle; ulContextValue:UInt64):Boolean; CDecl;
	ISteamGameServerHTTP_SetHTTPRequestNetworkActivityTimeout: Function(hRequest:SteamHTTPRequestHandle; unTimeoutSeconds:Cardinal):Boolean; CDecl;
	ISteamGameServerHTTP_SetHTTPRequestHeaderValue: Function(hRequest:SteamHTTPRequestHandle; pchHeaderName:PAnsiChar; pchHeaderValue:PAnsiChar):Boolean; CDecl;
	ISteamGameServerHTTP_SetHTTPRequestGetOrPostParameter: Function(hRequest:SteamHTTPRequestHandle; pchParamName:PAnsiChar; pchParamValue:PAnsiChar):Boolean; CDecl;
	ISteamGameServerHTTP_SendHTTPRequest: Function(hRequest:SteamHTTPRequestHandle; Var pCallHandle:SteamAPICall):Boolean; CDecl;
	ISteamGameServerHTTP_SendHTTPRequestAndStreamResponse: Function(hRequest:SteamHTTPRequestHandle; Var pCallHandle:SteamAPICall):Boolean; CDecl;
	ISteamGameServerHTTP_DeferHTTPRequest: Function(hRequest:SteamHTTPRequestHandle):Boolean; CDecl;
	ISteamGameServerHTTP_PrioritizeHTTPRequest: Function(hRequest:SteamHTTPRequestHandle):Boolean; CDecl;
	ISteamGameServerHTTP_GetHTTPResponseHeaderSize: Function(hRequest:SteamHTTPRequestHandle; pchHeaderName:PAnsiChar; Var unResponseHeaderSize:Cardinal):Boolean; CDecl;
	ISteamGameServerHTTP_GetHTTPResponseHeaderValue: Function(hRequest:SteamHTTPRequestHandle; pchHeaderName:PAnsiChar; pHeaderValueBuffer:PByte; unBufferSize:Cardinal):Boolean; CDecl;
	ISteamGameServerHTTP_GetHTTPResponseBodySize: Function(hRequest:SteamHTTPRequestHandle; Var unBodySize:Cardinal):Boolean; CDecl;
	ISteamGameServerHTTP_GetHTTPResponseBodyData: Function(hRequest:SteamHTTPRequestHandle; pBodyDataBuffer:PByte; unBufferSize:Cardinal):Boolean; CDecl;
	ISteamGameServerHTTP_GetHTTPStreamingResponseBodyData: Function(hRequest:SteamHTTPRequestHandle; cOffset:Cardinal; pBodyDataBuffer:PByte; unBufferSize:Cardinal):Boolean; CDecl;
	ISteamGameServerHTTP_ReleaseHTTPRequest: Function(hRequest:SteamHTTPRequestHandle):Boolean; CDecl;
	ISteamGameServerHTTP_GetHTTPDownloadProgressPct: Function(hRequest:SteamHTTPRequestHandle; Var pflPercentOut:Single):Boolean; CDecl;
	ISteamGameServerHTTP_SetHTTPRequestRawPostBody: Function(hRequest:SteamHTTPRequestHandle; pchContentType:PAnsiChar; pubBody:PByte; unBodyLen:Cardinal):Boolean; CDecl;

// SteamGameServerNetworking
Var
	ISteamGameServerNetworking_SendP2PPacket: Function(steamIDRemote:SteamID; pubData:PByte; cubData:Cardinal; eP2PSendType:SteamP2PSend; nChannel:Integer):Boolean; CDecl;
	ISteamGameServerNetworking_IsP2PPacketAvailable: Function(Var pcubMsgSize:Cardinal; nChannel:Integer = 0):Boolean; CDecl;
	ISteamGameServerNetworking_ReadP2PPacket: Function(pubDest:PByte; cubDest:Cardinal; Var pcubMsgSize:Cardinal; Var psteamIDRemote:SteamID; nChannel:Integer):Boolean; CDecl;
	ISteamGameServerNetworking_AcceptP2PSessionWithUser: Function(steamIDRemote:SteamID):Boolean; CDecl;
	ISteamGameServerNetworking_CloseP2PSessionWithUser: Function(steamIDRemote:SteamID):Boolean; CDecl;
	ISteamGameServerNetworking_CloseP2PChannelWithUser: Function(steamIDRemote:SteamID; nChannel:Integer):Boolean; CDecl;
	ISteamGameServerNetworking_GetP2PSessionState: Function(steamIDRemote:SteamID; Var pConnectionState:SteamP2PSessionState):Boolean; CDecl;
	ISteamGameServerNetworking_AllowP2PPacketRelay: Function(bAllow:Boolean):Boolean; CDecl;
	ISteamGameServerNetworking_CreateListenSocket: Function(nVirtualP2PPort:Integer; nIP:Cardinal; nPort:Word; bAllowUseOfPacketRelay:Boolean):Cardinal; CDecl;
	ISteamGameServerNetworking_CreateP2PConnectionSocket: Function(steamIDTarget:SteamID; nVirtualPort:Integer; nTimeoutSec:Integer; bAllowUseOfPacketRelay:Boolean):Cardinal; CDecl;
	ISteamGameServerNetworking_CreateConnectionSocket: Function(nIP:Cardinal; nPort:Word; nTimeoutSec:Integer):Cardinal; CDecl;
	ISteamGameServerNetworking_DestroySocket: Function(hSocket:SteamSNetSocket; bNotifyRemoteEnd:Boolean):Boolean; CDecl;
	ISteamGameServerNetworking_DestroyListenSocket: Function(hSocket:SteamSNetListenSocket; bNotifyRemoteEnd:Boolean):Boolean; CDecl;
	ISteamGameServerNetworking_SendDataOnSocket: Function(hSocket:SteamSNetSocket; pubData:Pointer; cubData:Cardinal; bReliable:Boolean):Boolean; CDecl;
	ISteamGameServerNetworking_IsDataAvailableOnSocket: Function(hSocket:SteamSNetSocket; Var pcubMsgSize:Cardinal):Boolean; CDecl;
	ISteamGameServerNetworking_RetrieveDataFromSocket: Function(hSocket:SteamSNetSocket; pubDest:Pointer; cubDest:Cardinal; Var pcubMsgSize:Cardinal):Boolean; CDecl;
	ISteamGameServerNetworking_IsDataAvailable: Function(hListenSocket:SteamSNetListenSocket; Var pcubMsgSize:Cardinal; Var phSocket:SteamSNetSocket):Boolean; CDecl;
	ISteamGameServerNetworking_RetrieveData: Function(hListenSocket:SteamSNetListenSocket; pubDest:Pointer; cubDest:Cardinal; Var pcubMsgSize:Cardinal; Var phSocket:SteamSNetSocket):Boolean; CDecl;
	ISteamGameServerNetworking_GetSocketInfo: Function(hSocket:SteamSNetSocket; Var pSteamIDRemote:SteamID; Var peSocketStatus:Integer; Var punIPRemote:Cardinal; Var punPortRemote:Word):Boolean; CDecl;
	ISteamGameServerNetworking_GetListenSocketInfo: Function(hListenSocket:SteamSNetListenSocket; Var pnIP:Cardinal; Var pnPort:Word):Boolean; CDecl;
	ISteamGameServerNetworking_GetSocketConnectionType: Function(hSocket:SteamSNetSocket):SteamSNetSocketConnectionType; CDecl;
	ISteamGameServerNetworking_GetMaxPacketSize: Function(hSocket:SteamSNetSocket):Integer; CDecl;

// SteamGameServerStats
Var
	ISteamGameServerStats_RequestUserStats: Function(steamIDUser:SteamID):UInt64; CDecl;
	ISteamGameServerStats_GetUserStat: Function(steamIDUser:SteamID; pchName:PAnsiChar; Var pData:Integer):Boolean; CDecl;
	ISteamGameServerStats_GetUserStat_: Function(steamIDUser:SteamID; pchName:PAnsiChar; Var pData:Single):Boolean; CDecl;
	ISteamGameServerStats_GetUserAchievement: Function(steamIDUser:SteamID; pchName:PAnsiChar; Var pbAchieved:Boolean):Boolean; CDecl;
	ISteamGameServerStats_SetUserStat: Function(steamIDUser:SteamID; pchName:PAnsiChar; nData:Integer):Boolean; CDecl;
	ISteamGameServerStats_SetUserStat_: Function(steamIDUser:SteamID; pchName:PAnsiChar; fData:Single):Boolean; CDecl;
	ISteamGameServerStats_UpdateUserAvgRateStat: Function(steamIDUser:SteamID; pchName:PAnsiChar; flCountThisSession:Single; dSessionLength:double):Boolean; CDecl;
	ISteamGameServerStats_SetUserAchievement: Function(steamIDUser:SteamID; pchName:PAnsiChar):Boolean; CDecl;
	ISteamGameServerStats_ClearUserAchievement: Function(steamIDUser:SteamID; pchName:PAnsiChar):Boolean; CDecl;
	ISteamGameServerStats_StoreUserStats: Function(steamIDUser:SteamID):UInt64; CDecl;

// SteamGameServerUtils
Var
	ISteamGameServerUtils_GetSecondsSinceAppActive: Function():Cardinal; CDecl;
	ISteamGameServerUtils_GetSecondsSinceComputerActive: Function():Cardinal; CDecl;
	ISteamGameServerUtils_GetConnectedUniverse: Function():SteamUniverse; CDecl;
	ISteamGameServerUtils_GetServerRealTime: Function():Cardinal; CDecl;
	ISteamGameServerUtils_GetIPCountry: Function():PAnsiChar; CDecl;
	ISteamGameServerUtils_GetImageSize: Function(iImage:Integer; Var pnWidth:Cardinal; Var pnHeight:Cardinal):Boolean; CDecl;
	ISteamGameServerUtils_GetImageRGBA: Function(iImage:Integer; pubDest:PByte; nDestBufferSize:Integer):Boolean; CDecl;
	ISteamGameServerUtils_GetCSERIPPort: Function(Var unIP:Cardinal; Var usPort:Word):Boolean; CDecl;
	ISteamGameServerUtils_GetCurrentBatteryPower: Function():Byte; CDecl;
	ISteamGameServerUtils_GetAppID: Function():Cardinal; CDecl;
	ISteamGameServerUtils_SetOverlayNotificationPosition: Procedure(eNotificationPosition:SteamNotificationPosition); CDecl;
	ISteamGameServerUtils_IsAPICallCompleted: Function(hSteamAPICall:SteamAPICall; Var pbFailed:Boolean):Boolean; CDecl;
	ISteamGameServerUtils_GetAPICallFailureReason: Function(hSteamAPICall:SteamAPICall):SteamAPICallFailure; CDecl;
	ISteamGameServerUtils_GetAPICallResult: Function(hSteamAPICall:SteamAPICall; pCallback:Pointer; cubCallback:Integer; iCallbackExpected:Integer; Var pbFailed:Boolean):Boolean; CDecl;
	ISteamGameServerUtils_RunFrame: Procedure(); CDecl;
	ISteamGameServerUtils_GetIPCCallCount: Function():Cardinal; CDecl;
	ISteamGameServerUtils_SetWarningMessageHook: Procedure(pFunction:SteamAPIWarningMessageHook); CDecl;
	ISteamGameServerUtils_IsOverlayEnabled: Function():Boolean; CDecl;
	ISteamGameServerUtils_BOverlayNeedsPresent: Function():Boolean; CDecl;
	ISteamGameServerUtils_ShowGamepadTextInput: Function(eInputMode:SteamGamepadTextInputMode; eLineInputMode:SteamGamepadTextInputLineMode; pchDescription:PAnsiChar; unCharMax:Cardinal; pchExistingText:PAnsiChar):Boolean; CDecl;
	ISteamGameServerUtils_GetEnteredGamepadTextLength: Function():Cardinal; CDecl;
	ISteamGameServerUtils_GetEnteredGamepadTextInput: Function(pchText:Pointer; cchText:Cardinal):Boolean; CDecl;
	ISteamGameServerUtils_GetSteamUILanguage: Function():PAnsiChar; CDecl;
	ISteamGameServerUtils_IsSteamRunningInVR: Function():Boolean; CDecl;

// SteamHTMLSurface
Var
	ISteamHTMLSurface_Init: Function():Boolean; CDecl;
	ISteamHTMLSurface_Shutdown: Function():Boolean; CDecl;
	ISteamHTMLSurface_CreateBrowser: Function(pchUserAgent:PAnsiChar; pchUserCSS:PAnsiChar):UInt64; CDecl;
	ISteamHTMLSurface_RemoveBrowser: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_LoadURL: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; pchURL:PAnsiChar; pchPostData:PAnsiChar); CDecl;
	ISteamHTMLSurface_SetSize: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; unWidth:Cardinal; unHeight:Cardinal); CDecl;
	ISteamHTMLSurface_StopLoad: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_Reload: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_GoBack: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_GoForward: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_AddHeader: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; pchKey:PAnsiChar; pchValue:PAnsiChar); CDecl;
	ISteamHTMLSurface_ExecuteJavascript: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; pchScript:PAnsiChar); CDecl;
	ISteamHTMLSurface_MouseUp: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; eMouseButton:SteamHTMLMouseButton); CDecl;
	ISteamHTMLSurface_MouseDown: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; eMouseButton:SteamHTMLMouseButton); CDecl;
	ISteamHTMLSurface_MouseDoubleClick: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; eMouseButton:SteamHTMLMouseButton); CDecl;
	ISteamHTMLSurface_MouseMove: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; x:Integer; y:Integer); CDecl;
	ISteamHTMLSurface_MouseWheel: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; nDelta:Integer); CDecl;
	ISteamHTMLSurface_KeyDown: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; nNativeKeyCode:Cardinal; eHTMLKeyModifiers:SteamHTMLKeyModifiers); CDecl;
	ISteamHTMLSurface_KeyUp: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; nNativeKeyCode:Cardinal; eHTMLKeyModifiers:SteamHTMLKeyModifiers); CDecl;
	ISteamHTMLSurface_KeyChar: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; cUnicodeChar:Cardinal; eHTMLKeyModifiers:SteamHTMLKeyModifiers); CDecl;
	ISteamHTMLSurface_SetHorizontalScroll: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; nAbsolutePixelScroll:Cardinal); CDecl;
	ISteamHTMLSurface_SetVerticalScroll: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; nAbsolutePixelScroll:Cardinal); CDecl;
	ISteamHTMLSurface_SetKeyFocus: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; bHasKeyFocus:Boolean); CDecl;
	ISteamHTMLSurface_ViewSource: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_CopyToClipboard: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_PasteFromClipboard: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_Find: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; pchSearchStr:PAnsiChar; bCurrentlyInFind:Boolean; bReverse:Boolean); CDecl;
	ISteamHTMLSurface_StopFind: Procedure(unBrowserHandle:SteamHTMLBrowserHandle); CDecl;
	ISteamHTMLSurface_GetLinkAtPosition: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; x:Integer; y:Integer); CDecl;
	ISteamHTMLSurface_SetCookie: Procedure(pchHostname:PAnsiChar; pchKey:PAnsiChar; pchValue:PAnsiChar; pchPath:PAnsiChar; nExpires:Cardinal = 0; bSecure:Boolean = false; bHTTPOnly:Boolean = false); CDecl;
	ISteamHTMLSurface_SetPageScaleFactor: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; flZoom:Single; nPointX:Integer; nPointY:Integer); CDecl;
	ISteamHTMLSurface_AllowStartRequest: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; bAllowed:Boolean); CDecl;
	ISteamHTMLSurface_JSDialogResponse: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; bResult:Boolean); CDecl;
	ISteamHTMLSurface_FileLoadDialogResponse: Procedure(unBrowserHandle:SteamHTMLBrowserHandle; pchSelectedFiles:Pointer); CDecl;

// SteamHTTP
Var
	ISteamHTTP_CreateHTTPRequest: Function(eHTTPRequestMethod:SteamHTTPMethod; pchAbsoluteURL:PAnsiChar):SteamHTTPRequestHandle; CDecl;
	ISteamHTTP_SetHTTPRequestContextValue: Function(hRequest:SteamHTTPRequestHandle; ulContextValue:UInt64):Boolean; CDecl;
	ISteamHTTP_SetHTTPRequestNetworkActivityTimeout: Function(hRequest:SteamHTTPRequestHandle; unTimeoutSeconds:Cardinal):Boolean; CDecl;
	ISteamHTTP_SetHTTPRequestHeaderValue: Function(hRequest:SteamHTTPRequestHandle; pchHeaderName:PAnsiChar; pchHeaderValue:PAnsiChar):Boolean; CDecl;
	ISteamHTTP_SetHTTPRequestGetOrPostParameter: Function(hRequest:SteamHTTPRequestHandle; pchParamName:PAnsiChar; pchParamValue:PAnsiChar):Boolean; CDecl;
	ISteamHTTP_SendHTTPRequest: Function(hRequest:SteamHTTPRequestHandle; Var pCallHandle:SteamAPICall):Boolean; CDecl;
	ISteamHTTP_SendHTTPRequestAndStreamResponse: Function(hRequest:SteamHTTPRequestHandle; Var pCallHandle:SteamAPICall):Boolean; CDecl;
	ISteamHTTP_DeferHTTPRequest: Function(hRequest:SteamHTTPRequestHandle):Boolean; CDecl;
	ISteamHTTP_PrioritizeHTTPRequest: Function(hRequest:SteamHTTPRequestHandle):Boolean; CDecl;
	ISteamHTTP_GetHTTPResponseHeaderSize: Function(hRequest:SteamHTTPRequestHandle; pchHeaderName:PAnsiChar; Var unResponseHeaderSize:Cardinal):Boolean; CDecl;
	ISteamHTTP_GetHTTPResponseHeaderValue: Function(hRequest:SteamHTTPRequestHandle; pchHeaderName:PAnsiChar; pHeaderValueBuffer:PByte; unBufferSize:Cardinal):Boolean; CDecl;
	ISteamHTTP_GetHTTPResponseBodySize: Function(hRequest:SteamHTTPRequestHandle; Var unBodySize:Cardinal):Boolean; CDecl;
	ISteamHTTP_GetHTTPResponseBodyData: Function(hRequest:SteamHTTPRequestHandle; pBodyDataBuffer:PByte; unBufferSize:Cardinal):Boolean; CDecl;
	ISteamHTTP_GetHTTPStreamingResponseBodyData: Function(hRequest:SteamHTTPRequestHandle; cOffset:Cardinal; pBodyDataBuffer:PByte; unBufferSize:Cardinal):Boolean; CDecl;
	ISteamHTTP_ReleaseHTTPRequest: Function(hRequest:SteamHTTPRequestHandle):Boolean; CDecl;
	ISteamHTTP_GetHTTPDownloadProgressPct: Function(hRequest:SteamHTTPRequestHandle; Var pflPercentOut:Single):Boolean; CDecl;
	ISteamHTTP_SetHTTPRequestRawPostBody: Function(hRequest:SteamHTTPRequestHandle; pchContentType:PAnsiChar; pubBody:PByte; unBodyLen:Cardinal):Boolean; CDecl;

// SteamMatchmaking
Var
	ISteamMatchmaking_GetFavoriteGameCount: Function():Integer; CDecl;
	ISteamMatchmaking_GetFavoriteGame: Function(iGame:Integer; Var pnAppID:SteamAppId; Var pnIP:Cardinal; Var pnConnPort:Word; Var pnQueryPort:Word; Var punFlags:Cardinal; Var pRTime32LastPlayedOnServer:Cardinal):Boolean; CDecl;
	ISteamMatchmaking_AddFavoriteGame: Function(nAppID:SteamAppId; nIP:Cardinal; nConnPort:Word; nQueryPort:Word; unFlags:Cardinal; rTime32LastPlayedOnServer:Cardinal):Integer; CDecl;
	ISteamMatchmaking_RemoveFavoriteGame: Function(nAppID:SteamAppId; nIP:Cardinal; nConnPort:Word; nQueryPort:Word; unFlags:Cardinal):Boolean; CDecl;
	ISteamMatchmaking_RequestLobbyList: Function():UInt64; CDecl;
	ISteamMatchmaking_AddRequestLobbyListStringFilter: Procedure(pchKeyToMatch:PAnsiChar; pchValueToMatch:PAnsiChar; eComparisonType:SteamLobbyComparison); CDecl;
	ISteamMatchmaking_AddRequestLobbyListNumericalFilter: Procedure(pchKeyToMatch:PAnsiChar; nValueToMatch:Integer; eComparisonType:SteamLobbyComparison); CDecl;
	ISteamMatchmaking_AddRequestLobbyListNearValueFilter: Procedure(pchKeyToMatch:PAnsiChar; nValueToBeCloseTo:Integer); CDecl;
	ISteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable: Procedure(nSlotsAvailable:Integer); CDecl;
	ISteamMatchmaking_AddRequestLobbyListDistanceFilter: Procedure(eLobbyDistanceFilter:SteamLobbyDistanceFilter); CDecl;
	ISteamMatchmaking_AddRequestLobbyListResultCountFilter: Procedure(cMaxResults:Integer); CDecl;
	ISteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter: Procedure(steamIDLobby:SteamID); CDecl;
	ISteamMatchmaking_GetLobbyByIndex: Function(iLobby:Integer):UInt64; CDecl;
	ISteamMatchmaking_CreateLobby: Function(eLobbyType:SteamLobbyType; cMaxMembers:Integer):UInt64; CDecl;
	ISteamMatchmaking_JoinLobby: Function(steamIDLobby:SteamID):UInt64; CDecl;
	ISteamMatchmaking_LeaveLobby: Procedure(steamIDLobby:SteamID); CDecl;
	ISteamMatchmaking_InviteUserToLobby: Function(steamIDLobby:SteamID; steamIDInvitee:SteamID):Boolean; CDecl;
	ISteamMatchmaking_GetNumLobbyMembers: Function(steamIDLobby:SteamID):Integer; CDecl;
	ISteamMatchmaking_GetLobbyMemberByIndex: Function(steamIDLobby:SteamID; iMember:Integer):UInt64; CDecl;
	ISteamMatchmaking_GetLobbyData: Function(steamIDLobby:SteamID; pchKey:PAnsiChar):PAnsiChar; CDecl;
	ISteamMatchmaking_SetLobbyData: Function(steamIDLobby:SteamID; pchKey:PAnsiChar; pchValue:PAnsiChar):Boolean; CDecl;
	ISteamMatchmaking_GetLobbyDataCount: Function(steamIDLobby:SteamID):Integer; CDecl;
	ISteamMatchmaking_GetLobbyDataByIndex: Function(steamIDLobby:SteamID; iLobbyData:Integer; pchKey:Pointer; cchKeyBufferSize:Integer; pchValue:Pointer; cchValueBufferSize:Integer):Boolean; CDecl;
	ISteamMatchmaking_DeleteLobbyData: Function(steamIDLobby:SteamID; pchKey:PAnsiChar):Boolean; CDecl;
	ISteamMatchmaking_GetLobbyMemberData: Function(steamIDLobby:SteamID; steamIDUser:SteamID; pchKey:PAnsiChar):PAnsiChar; CDecl;
	ISteamMatchmaking_SetLobbyMemberData: Procedure(steamIDLobby:SteamID; pchKey:PAnsiChar; pchValue:PAnsiChar); CDecl;
	ISteamMatchmaking_SendLobbyChatMsg: Function(steamIDLobby:SteamID; pvMsgBody:PByte; cubMsgBody:Integer):Boolean; CDecl;
	ISteamMatchmaking_GetLobbyChatEntry: Function(steamIDLobby:SteamID; iChatID:Integer; Var pSteamIDUser:SteamID; pvData:PByte; cubData:Integer; Var peChatEntryType:SteamChatEntryType):Integer; CDecl;
	ISteamMatchmaking_RequestLobbyData: Function(steamIDLobby:SteamID):Boolean; CDecl;
	ISteamMatchmaking_SetLobbyGameServer: Procedure(steamIDLobby:SteamID; unGameServerIP:Cardinal; unGameServerPort:Word; steamIDGameServer:SteamID); CDecl;
	ISteamMatchmaking_GetLobbyGameServer: Function(steamIDLobby:SteamID; Var punGameServerIP:Cardinal; Var punGameServerPort:Word; Var psteamIDGameServer:SteamID):Boolean; CDecl;
	ISteamMatchmaking_SetLobbyMemberLimit: Function(steamIDLobby:SteamID; cMaxMembers:Integer):Boolean; CDecl;
	ISteamMatchmaking_GetLobbyMemberLimit: Function(steamIDLobby:SteamID):Integer; CDecl;
	ISteamMatchmaking_SetLobbyType: Function(steamIDLobby:SteamID; eLobbyType:SteamLobbyType):Boolean; CDecl;
	ISteamMatchmaking_SetLobbyJoinable: Function(steamIDLobby:SteamID; bLobbyJoinable:Boolean):Boolean; CDecl;
	ISteamMatchmaking_GetLobbyOwner: Function(steamIDLobby:SteamID):UInt64; CDecl;
	ISteamMatchmaking_SetLobbyOwner: Function(steamIDLobby:SteamID; steamIDNewOwner:SteamID):Boolean; CDecl;
	ISteamMatchmaking_SetLinkedLobby: Function(steamIDLobby:SteamID; steamIDLobbyDependent:SteamID):Boolean; CDecl;

// SteamMatchmakingServers
Var
	ISteamMatchmakingServers_RequestInternetServerList: Function(iApp:SteamAppId; ppchFilters:Pointer; nFilters:Cardinal; pRequestServersResponse:Pointer):Pointer; CDecl;
	ISteamMatchmakingServers_RequestLANServerList: Function(iApp:SteamAppId; pRequestServersResponse:Pointer):Pointer; CDecl;
	ISteamMatchmakingServers_RequestFriendsServerList: Function(iApp:SteamAppId; ppchFilters:Pointer; nFilters:Cardinal; pRequestServersResponse:Pointer):Pointer; CDecl;
	ISteamMatchmakingServers_RequestFavoritesServerList: Function(iApp:SteamAppId; ppchFilters:Pointer; nFilters:Cardinal; pRequestServersResponse:Pointer):Pointer; CDecl;
	ISteamMatchmakingServers_RequestHistoryServerList: Function(iApp:SteamAppId; ppchFilters:Pointer; nFilters:Cardinal; pRequestServersResponse:Pointer):Pointer; CDecl;
	ISteamMatchmakingServers_RequestSpectatorServerList: Function(iApp:SteamAppId; ppchFilters:Pointer; nFilters:Cardinal; pRequestServersResponse:Pointer):Pointer; CDecl;
	ISteamMatchmakingServers_ReleaseRequest: Procedure(hServerListRequest:SteamServerListRequestHandle); CDecl;
	ISteamMatchmakingServers_GetServerDetails: Function(hRequest:SteamServerListRequestHandle; iServer:Integer):Pointer; CDecl;
	ISteamMatchmakingServers_CancelQuery: Procedure(hRequest:SteamServerListRequestHandle); CDecl;
	ISteamMatchmakingServers_RefreshQuery: Procedure(hRequest:SteamServerListRequestHandle); CDecl;
	ISteamMatchmakingServers_IsRefreshing: Function(hRequest:SteamServerListRequestHandle):Boolean; CDecl;
	ISteamMatchmakingServers_GetServerCount: Function(hRequest:SteamServerListRequestHandle):Integer; CDecl;
	ISteamMatchmakingServers_RefreshServer: Procedure(hRequest:SteamServerListRequestHandle; iServer:Integer); CDecl;
	ISteamMatchmakingServers_PingServer: Function(unIP:Cardinal; usPort:Word; pRequestServersResponse:Pointer):Integer; CDecl;
	ISteamMatchmakingServers_PlayerDetails: Function(unIP:Cardinal; usPort:Word; pRequestServersResponse:Pointer):Integer; CDecl;
	ISteamMatchmakingServers_ServerRules: Function(unIP:Cardinal; usPort:Word; pRequestServersResponse:Pointer):Integer; CDecl;
	ISteamMatchmakingServers_CancelServerQuery: Procedure(hServerQuery:SteamServerQueryHandle); CDecl;

// SteamMusic
Var
	ISteamMusic_BIsEnabled: Function():Boolean; CDecl;
	ISteamMusic_BIsPlaying: Function():Boolean; CDecl;
	ISteamMusic_GetPlaybackStatus: Function():SteamAudioPlayback_Status; CDecl;
	ISteamMusic_Play: Procedure(); CDecl;
	ISteamMusic_Pause: Procedure(); CDecl;
	ISteamMusic_PlayPrevious: Procedure(); CDecl;
	ISteamMusic_PlayNext: Procedure(); CDecl;
	ISteamMusic_SetVolume: Procedure(flVolume:Single); CDecl;
	ISteamMusic_GetVolume: Function():Single; CDecl;

// SteamMusicRemote
Var
	ISteamMusicRemote_RegisterSteamMusicRemote: Function(pchName:PAnsiChar):Boolean; CDecl;
	ISteamMusicRemote_DeregisterSteamMusicRemote: Function():Boolean; CDecl;
	ISteamMusicRemote_BIsCurrentMusicRemote: Function():Boolean; CDecl;
	ISteamMusicRemote_BActivationSuccess: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_SetDisplayName: Function(pchDisplayName:PAnsiChar):Boolean; CDecl;
	ISteamMusicRemote_SetPNGIcon_64x64: Function(pvBuffer:PByte; cbBufferLength:Cardinal):Boolean; CDecl;
	ISteamMusicRemote_EnablePlayPrevious: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_EnablePlayNext: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_EnableShuffled: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_EnableLooped: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_EnableQueue: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_EnablePlaylists: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_UpdatePlaybackStatus: Function(nStatus:SteamAudioPlayback_Status):Boolean; CDecl;
	ISteamMusicRemote_UpdateShuffled: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_UpdateLooped: Function(bValue:Boolean):Boolean; CDecl;
	ISteamMusicRemote_UpdateVolume: Function(flValue:Single):Boolean; CDecl;
	ISteamMusicRemote_CurrentEntryWillChange: Function():Boolean; CDecl;
	ISteamMusicRemote_CurrentEntryIsAvailable: Function(bAvailable:Boolean):Boolean; CDecl;
	ISteamMusicRemote_UpdateCurrentEntryText: Function(pchText:PAnsiChar):Boolean; CDecl;
	ISteamMusicRemote_UpdateCurrentEntryElapsedSeconds: Function(nValue:Integer):Boolean; CDecl;
	ISteamMusicRemote_UpdateCurrentEntryCoverArt: Function(pvBuffer:PByte; cbBufferLength:Cardinal):Boolean; CDecl;
	ISteamMusicRemote_CurrentEntryDidChange: Function():Boolean; CDecl;
	ISteamMusicRemote_QueueWillChange: Function():Boolean; CDecl;
	ISteamMusicRemote_ResetQueueEntries: Function():Boolean; CDecl;
	ISteamMusicRemote_SetQueueEntry: Function(nID:Integer; nPosition:Integer; pchEntryText:PAnsiChar):Boolean; CDecl;
	ISteamMusicRemote_SetCurrentQueueEntry: Function(nID:Integer):Boolean; CDecl;
	ISteamMusicRemote_QueueDidChange: Function():Boolean; CDecl;
	ISteamMusicRemote_PlaylistWillChange: Function():Boolean; CDecl;
	ISteamMusicRemote_ResetPlaylistEntries: Function():Boolean; CDecl;
	ISteamMusicRemote_SetPlaylistEntry: Function(nID:Integer; nPosition:Integer; pchEntryText:PAnsiChar):Boolean; CDecl;
	ISteamMusicRemote_SetCurrentPlaylistEntry: Function(nID:Integer):Boolean; CDecl;
	ISteamMusicRemote_PlaylistDidChange: Function():Boolean; CDecl;

// SteamNetworking
Var
	ISteamNetworking_SendP2PPacket: Function(steamIDRemote:SteamID; pubData:PByte; cubData:Cardinal; eP2PSendType:SteamP2PSend; nChannel:Integer):Boolean; CDecl;
	ISteamNetworking_IsP2PPacketAvailable: Function(Var pcubMsgSize:Cardinal; nChannel:Integer = 0):Boolean; CDecl;
	ISteamNetworking_ReadP2PPacket: Function(pubDest:PByte; cubDest:Cardinal; Var pcubMsgSize:Cardinal; Var psteamIDRemote:SteamID; nChannel:Integer):Boolean; CDecl;
	ISteamNetworking_AcceptP2PSessionWithUser: Function(steamIDRemote:SteamID):Boolean; CDecl;
	ISteamNetworking_CloseP2PSessionWithUser: Function(steamIDRemote:SteamID):Boolean; CDecl;
	ISteamNetworking_CloseP2PChannelWithUser: Function(steamIDRemote:SteamID; nChannel:Integer):Boolean; CDecl;
	ISteamNetworking_GetP2PSessionState: Function(steamIDRemote:SteamID; Var pConnectionState:SteamP2PSessionState):Boolean; CDecl;
	ISteamNetworking_AllowP2PPacketRelay: Function(bAllow:Boolean):Boolean; CDecl;
	ISteamNetworking_CreateListenSocket: Function(nVirtualP2PPort:Integer; nIP:Cardinal; nPort:Word; bAllowUseOfPacketRelay:Boolean):Cardinal; CDecl;
	ISteamNetworking_CreateP2PConnectionSocket: Function(steamIDTarget:SteamID; nVirtualPort:Integer; nTimeoutSec:Integer; bAllowUseOfPacketRelay:Boolean):Cardinal; CDecl;
	ISteamNetworking_CreateConnectionSocket: Function(nIP:Cardinal; nPort:Word; nTimeoutSec:Integer):Cardinal; CDecl;
	ISteamNetworking_DestroySocket: Function(hSocket:SteamSNetSocket; bNotifyRemoteEnd:Boolean):Boolean; CDecl;
	ISteamNetworking_DestroyListenSocket: Function(hSocket:SteamSNetListenSocket; bNotifyRemoteEnd:Boolean):Boolean; CDecl;
	ISteamNetworking_SendDataOnSocket: Function(hSocket:SteamSNetSocket; pubData:Pointer; cubData:Cardinal; bReliable:Boolean):Boolean; CDecl;
	ISteamNetworking_IsDataAvailableOnSocket: Function(hSocket:SteamSNetSocket; Var pcubMsgSize:Cardinal):Boolean; CDecl;
	ISteamNetworking_RetrieveDataFromSocket: Function(hSocket:SteamSNetSocket; pubDest:Pointer; cubDest:Cardinal; Var pcubMsgSize:Cardinal):Boolean; CDecl;
	ISteamNetworking_IsDataAvailable: Function(hListenSocket:SteamSNetListenSocket; Var pcubMsgSize:Cardinal; Var phSocket:SteamSNetSocket):Boolean; CDecl;
	ISteamNetworking_RetrieveData: Function(hListenSocket:SteamSNetListenSocket; pubDest:Pointer; cubDest:Cardinal; Var pcubMsgSize:Cardinal; Var phSocket:SteamSNetSocket):Boolean; CDecl;
	ISteamNetworking_GetSocketInfo: Function(hSocket:SteamSNetSocket; Var pSteamIDRemote:SteamID; Var peSocketStatus:Integer; Var punIPRemote:Cardinal; Var punPortRemote:Word):Boolean; CDecl;
	ISteamNetworking_GetListenSocketInfo: Function(hListenSocket:SteamSNetListenSocket; Var pnIP:Cardinal; Var pnPort:Word):Boolean; CDecl;
	ISteamNetworking_GetSocketConnectionType: Function(hSocket:SteamSNetSocket):SteamSNetSocketConnectionType; CDecl;
	ISteamNetworking_GetMaxPacketSize: Function(hSocket:SteamSNetSocket):Integer; CDecl;

// SteamRemoteStorage
Var
	ISteamRemoteStorage_FileWrite: Function(pchFile:PAnsiChar; pvData:PByte; cubData:Integer):Boolean; CDecl;
	ISteamRemoteStorage_FileRead: Function(pchFile:PAnsiChar; pvData:PByte; cubDataToRead:Integer):Integer; CDecl;
	ISteamRemoteStorage_FileForget: Function(pchFile:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_FileDelete: Function(pchFile:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_FileShare: Function(pchFile:PAnsiChar):UInt64; CDecl;
	ISteamRemoteStorage_SetSyncPlatforms: Function(pchFile:PAnsiChar; eRemoteStoragePlatform:SteamRemoteStoragePlatform):Boolean; CDecl;
	ISteamRemoteStorage_FileWriteStreamOpen: Function(pchFile:PAnsiChar):UInt64; CDecl;
	ISteamRemoteStorage_FileWriteStreamWriteChunk: Function(writeHandle:SteamUGCFileWriteStreamHandle; pvData:PByte; cubData:Integer):Boolean; CDecl;
	ISteamRemoteStorage_FileWriteStreamClose: Function(writeHandle:SteamUGCFileWriteStreamHandle):Boolean; CDecl;
	ISteamRemoteStorage_FileWriteStreamCancel: Function(writeHandle:SteamUGCFileWriteStreamHandle):Boolean; CDecl;
	ISteamRemoteStorage_FileExists: Function(pchFile:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_FilePersisted: Function(pchFile:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_GetFileSize: Function(pchFile:PAnsiChar):Integer; CDecl;
	ISteamRemoteStorage_GetFileTimestamp: Function(pchFile:PAnsiChar):Int64; CDecl;
	ISteamRemoteStorage_GetSyncPlatforms: Function(pchFile:PAnsiChar):SteamRemoteStoragePlatform; CDecl;
	ISteamRemoteStorage_GetFileCount: Function():Integer; CDecl;
	ISteamRemoteStorage_GetFileNameAndSize: Function(iFile:Integer; Var pnFileSizeInBytes:Integer):PAnsiChar; CDecl;
	ISteamRemoteStorage_GetQuota: Function(Var pnTotalBytes:Integer; Var puAvailableBytes:Integer):Boolean; CDecl;
	ISteamRemoteStorage_IsCloudEnabledForAccount: Function():Boolean; CDecl;
	ISteamRemoteStorage_IsCloudEnabledForApp: Function():Boolean; CDecl;
	ISteamRemoteStorage_SetCloudEnabledForApp: Procedure(bEnabled:Boolean); CDecl;
	ISteamRemoteStorage_UGCDownload: Function(hContent:SteamUGCHandle; unPriority:Cardinal):UInt64; CDecl;
	ISteamRemoteStorage_GetUGCDownloadProgress: Function(hContent:SteamUGCHandle; Var pnBytesDownloaded:Integer; Var pnBytesExpected:Integer):Boolean; CDecl;
	ISteamRemoteStorage_GetUGCDetails: Function(hContent:SteamUGCHandle; Var pnAppID:SteamAppId; Var ppchName:Pointer; Var pnFileSizeInBytes:Integer; Var pSteamIDOwner:SteamID):Boolean; CDecl;
	ISteamRemoteStorage_UGCRead: Function(hContent:SteamUGCHandle; pvData:PByte; cubDataToRead:Integer; cOffset:Cardinal; eAction:SteamUGCReadAction):Integer; CDecl;
	ISteamRemoteStorage_GetCachedUGCCount: Function():Integer; CDecl;
	ISteamRemoteStorage_GetCachedUGCHandle: Function(iCachedContent:Integer):UInt64; CDecl;
	ISteamRemoteStorage_PublishWorkshopFile: Function(pchFile:PAnsiChar; pchPreviewFile:PAnsiChar; nConsumerAppId:SteamAppId; pchTitle:PAnsiChar; pchDescription:PAnsiChar; eVisibility:SteamRemoteStoragePublishedFileVisibility; pTags:Pointer; eWorkshopFileType:SteamWorkshopFileType):UInt64; CDecl;
	ISteamRemoteStorage_CreatePublishedFileUpdateRequest: Function(unPublishedFileId:SteamPublishedFileId):UInt64; CDecl;
	ISteamRemoteStorage_UpdatePublishedFileFile: Function(updateHandle:SteamPublishedFileUpdateHandle; pchFile:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_UpdatePublishedFilePreviewFile: Function(updateHandle:SteamPublishedFileUpdateHandle; pchPreviewFile:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_UpdatePublishedFileTitle: Function(updateHandle:SteamPublishedFileUpdateHandle; pchTitle:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_UpdatePublishedFileDescription: Function(updateHandle:SteamPublishedFileUpdateHandle; pchDescription:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_UpdatePublishedFileVisibility: Function(updateHandle:SteamPublishedFileUpdateHandle; eVisibility:SteamRemoteStoragePublishedFileVisibility):Boolean; CDecl;
	ISteamRemoteStorage_UpdatePublishedFileTags: Function(updateHandle:SteamPublishedFileUpdateHandle; pTags:Pointer):Boolean; CDecl;
	ISteamRemoteStorage_CommitPublishedFileUpdate: Function(updateHandle:SteamPublishedFileUpdateHandle):UInt64; CDecl;
	ISteamRemoteStorage_GetPublishedFileDetails: Function(unPublishedFileId:SteamPublishedFileId; unMaxSecondsOld:Cardinal):UInt64; CDecl;
	ISteamRemoteStorage_DeletePublishedFile: Function(unPublishedFileId:SteamPublishedFileId):UInt64; CDecl;
	ISteamRemoteStorage_EnumerateUserPublishedFiles: Function(unStartIndex:Cardinal):UInt64; CDecl;
	ISteamRemoteStorage_SubscribePublishedFile: Function(unPublishedFileId:SteamPublishedFileId):UInt64; CDecl;
	ISteamRemoteStorage_EnumerateUserSubscribedFiles: Function(unStartIndex:Cardinal):UInt64; CDecl;
	ISteamRemoteStorage_UnsubscribePublishedFile: Function(unPublishedFileId:SteamPublishedFileId):UInt64; CDecl;
	ISteamRemoteStorage_UpdatePublishedFileSetChangeDescription: Function(updateHandle:SteamPublishedFileUpdateHandle; pchChangeDescription:PAnsiChar):Boolean; CDecl;
	ISteamRemoteStorage_GetPublishedItemVoteDetails: Function(unPublishedFileId:SteamPublishedFileId):UInt64; CDecl;
	ISteamRemoteStorage_UpdateUserPublishedItemVote: Function(unPublishedFileId:SteamPublishedFileId; bVoteUp:Boolean):UInt64; CDecl;
	ISteamRemoteStorage_GetUserPublishedItemVoteDetails: Function(unPublishedFileId:SteamPublishedFileId):UInt64; CDecl;
	ISteamRemoteStorage_EnumerateUserSharedWorkshopFiles: Function(steamId:SteamID; unStartIndex:Cardinal; pRequiredTags:Pointer; pExcludedTags:Pointer):UInt64; CDecl;
	ISteamRemoteStorage_PublishVideo: Function(eVideoProvider:SteamWorkshopVideoProvider; pchVideoAccount:PAnsiChar; pchVideoIdentifier:PAnsiChar; pchPreviewFile:PAnsiChar; nConsumerAppId:SteamAppId; pchTitle:PAnsiChar; pchDescription:PAnsiChar; eVisibility:SteamRemoteStoragePublishedFileVisibility; pTags:Pointer):UInt64; CDecl;
	ISteamRemoteStorage_SetUserPublishedFileAction: Function(unPublishedFileId:SteamPublishedFileId; eAction:SteamWorkshopFileAction):UInt64; CDecl;
	ISteamRemoteStorage_EnumeratePublishedFilesByUserAction: Function(eAction:SteamWorkshopFileAction; unStartIndex:Cardinal):UInt64; CDecl;
	ISteamRemoteStorage_EnumeratePublishedWorkshopFiles: Function(eEnumerationType:SteamWorkshopEnumerationType; unStartIndex:Cardinal; unCount:Cardinal; unDays:Cardinal; pTags:Pointer; pUserTags:Pointer):UInt64; CDecl;
	ISteamRemoteStorage_UGCDownloadToLocation: Function(hContent:SteamUGCHandle; pchLocation:PAnsiChar; unPriority:Cardinal):UInt64; CDecl;

// SteamScreenshots
Var
	ISteamScreenshots_WriteScreenshot: Function(pubRGB:PByte; cubRGB:Cardinal; nWidth:Integer; nHeight:Integer):Cardinal; CDecl;
	ISteamScreenshots_AddScreenshotToLibrary: Function(pchFilename:PAnsiChar; pchThumbnailFilename:PAnsiChar; nWidth:Integer; nHeight:Integer):Cardinal; CDecl;
	ISteamScreenshots_TriggerScreenshot: Procedure(); CDecl;
	ISteamScreenshots_HookScreenshots: Procedure(bHook:Boolean); CDecl;
	ISteamScreenshots_SetLocation: Function(hScreenshot:SteamScreenshotHandle; pchLocation:PAnsiChar):Boolean; CDecl;
	ISteamScreenshots_TagUser: Function(hScreenshot:SteamScreenshotHandle; steamID:SteamID):Boolean; CDecl;
	ISteamScreenshots_TagPublishedFile: Function(hScreenshot:SteamScreenshotHandle; unPublishedFileID:SteamPublishedFileId):Boolean; CDecl;

// SteamUGC
Var
	ISteamUGC_CreateQueryUserUGCRequest: Function(unAccountID:SteamAccountID; eListType:SteamUserUGCList; eMatchingUGCType:SteamUGCMatchingUGCType; eSortOrder:SteamUserUGCListSortOrder; nCreatorAppID:SteamAppId; nConsumerAppID:SteamAppId; unPage:Cardinal):UInt64; CDecl;
	ISteamUGC_CreateQueryAllUGCRequest: Function(eQueryType:SteamUGCQuery; eMatchingeMatchingUGCTypeFileType:SteamUGCMatchingUGCType; nCreatorAppID:SteamAppId; nConsumerAppID:SteamAppId; unPage:Cardinal):UInt64; CDecl;
	ISteamUGC_SendQueryUGCRequest: Function(handle:SteamUGCQueryHandle):UInt64; CDecl;
	ISteamUGC_GetQueryUGCResult: Function(handle:SteamUGCQueryHandle; index:Cardinal; Var pDetails:SteamUGCDetails):Boolean; CDecl;
	ISteamUGC_ReleaseQueryUGCRequest: Function(handle:SteamUGCQueryHandle):Boolean; CDecl;
	ISteamUGC_AddRequiredTag: Function(handle:SteamUGCQueryHandle; pTagName:PAnsiChar):Boolean; CDecl;
	ISteamUGC_AddExcludedTag: Function(handle:SteamUGCQueryHandle; pTagName:PAnsiChar):Boolean; CDecl;
	ISteamUGC_SetReturnLongDescription: Function(handle:SteamUGCQueryHandle; bReturnLongDescription:Boolean):Boolean; CDecl;
	ISteamUGC_SetReturnTotalOnly: Function(handle:SteamUGCQueryHandle; bReturnTotalOnly:Boolean):Boolean; CDecl;
	ISteamUGC_SetAllowCachedResponse: Function(handle:SteamUGCQueryHandle; unMaxAgeSeconds:Cardinal):Boolean; CDecl;
	ISteamUGC_SetCloudFileNameFilter: Function(handle:SteamUGCQueryHandle; pMatchCloudFileName:PAnsiChar):Boolean; CDecl;
	ISteamUGC_SetMatchAnyTag: Function(handle:SteamUGCQueryHandle; bMatchAnyTag:Boolean):Boolean; CDecl;
	ISteamUGC_SetSearchText: Function(handle:SteamUGCQueryHandle; pSearchText:PAnsiChar):Boolean; CDecl;
	ISteamUGC_SetRankedByTrendDays: Function(handle:SteamUGCQueryHandle; unDays:Cardinal):Boolean; CDecl;
	ISteamUGC_RequestUGCDetails: Function(nPublishedFileID:SteamPublishedFileId; unMaxAgeSeconds:Cardinal):UInt64; CDecl;
	ISteamUGC_CreateItem: Function(nConsumerAppId:SteamAppId; eFileType:SteamWorkshopFileType):UInt64; CDecl;
	ISteamUGC_StartItemUpdate: Function(nConsumerAppId:SteamAppId; nPublishedFileID:SteamPublishedFileId):UInt64; CDecl;
	ISteamUGC_SetItemTitle: Function(handle:SteamUGCUpdateHandle; pchTitle:PAnsiChar):Boolean; CDecl;
	ISteamUGC_SetItemDescription: Function(handle:SteamUGCUpdateHandle; pchDescription:PAnsiChar):Boolean; CDecl;
	ISteamUGC_SetItemVisibility: Function(handle:SteamUGCUpdateHandle; eVisibility:SteamRemoteStoragePublishedFileVisibility):Boolean; CDecl;
	ISteamUGC_SetItemTags: Function(updateHandle:SteamUGCUpdateHandle; pTags:Pointer):Boolean; CDecl;
	ISteamUGC_SetItemContent: Function(handle:SteamUGCUpdateHandle; pszContentFolder:PAnsiChar):Boolean; CDecl;
	ISteamUGC_SetItemPreview: Function(handle:SteamUGCUpdateHandle; pszPreviewFile:PAnsiChar):Boolean; CDecl;
	ISteamUGC_SubmitItemUpdate: Function(handle:SteamUGCUpdateHandle; pchChangeNote:PAnsiChar):UInt64; CDecl;
	ISteamUGC_GetItemUpdateProgress: Function(handle:SteamUGCUpdateHandle; Var punBytesProcessed:UInt64; Var punBytesTotal:UInt64):SteamItemUpdateStatus; CDecl;
	ISteamUGC_SubscribeItem: Function(nPublishedFileID:SteamPublishedFileId):UInt64; CDecl;
	ISteamUGC_UnsubscribeItem: Function(nPublishedFileID:SteamPublishedFileId):UInt64; CDecl;
	ISteamUGC_GetNumSubscribedItems: Function():Cardinal; CDecl;
	ISteamUGC_GetSubscribedItems: Function(pvecPublishedFileID:PSteamPublishedFileId; cMaxEntries:Cardinal):Cardinal; CDecl;
	ISteamUGC_GetItemInstallInfo: Function(nPublishedFileID:SteamPublishedFileId; Var punSizeOnDisk:UInt64; pchFolder:Pointer; cchFolderSize:Cardinal; Var pbLegacyItem:Boolean):Boolean; CDecl;
	ISteamUGC_GetItemUpdateInfo: Function(nPublishedFileID:SteamPublishedFileId; Var pbNeedsUpdate:Boolean; Var pbIsDownloading:Boolean; Var punBytesDownloaded:UInt64; Var punBytesTotal:UInt64):Boolean; CDecl;

// SteamUnifiedMessages
Var
	ISteamUnifiedMessages_SendMethod: Function(pchServiceMethod:PAnsiChar; pRequestBuffer:PByte; unRequestBufferSize:Cardinal; unContext:UInt64):UInt64; CDecl;
	ISteamUnifiedMessages_GetMethodResponseInfo: Function(hHandle:SteamClientUnifiedMessageHandle; Var punResponseSize:Cardinal; Var peResult:SteamResult):Boolean; CDecl;
	ISteamUnifiedMessages_GetMethodResponseData: Function(hHandle:SteamClientUnifiedMessageHandle; pResponseBuffer:PByte; unResponseBufferSize:Cardinal; bAutoRelease:Boolean):Boolean; CDecl;
	ISteamUnifiedMessages_ReleaseMethod: Function(hHandle:SteamClientUnifiedMessageHandle):Boolean; CDecl;
	ISteamUnifiedMessages_SendNotification: Function(pchServiceNotification:PAnsiChar; pNotificationBuffer:PByte; unNotificationBufferSize:Cardinal):Boolean; CDecl;

// SteamUser
Var
	ISteamUser_GetHSteamUser: Function():Integer; CDecl;
	ISteamUser_BLoggedOn: Function():Boolean; CDecl;
	ISteamUser_GetSteamID: Function():UInt64; CDecl;
	ISteamUser_InitiateGameConnection: Function(pAuthBlob:PByte; cbMaxAuthBlob:Integer; steamIDGameServer:SteamID; unIPServer:Cardinal; usPortServer:Word; bSecure:Boolean):Integer; CDecl;
	ISteamUser_TerminateGameConnection: Procedure(unIPServer:Cardinal; usPortServer:Word); CDecl;
	ISteamUser_TrackAppUsageEvent: Procedure(gameID:SteamGameID; eAppUsageEvent:Integer; pchExtraInfo:PAnsiChar); CDecl;
	ISteamUser_GetUserDataFolder: Function(pchBuffer:Pointer; cubBuffer:Integer):Boolean; CDecl;
	ISteamUser_StartVoiceRecording: Procedure(); CDecl;
	ISteamUser_StopVoiceRecording: Procedure(); CDecl;
	ISteamUser_GetAvailableVoice: Function(Var pcbCompressed:Cardinal; Var pcbUncompressed:Cardinal; nUncompressedVoiceDesiredSampleRate:Cardinal):SteamVoiceResult; CDecl;
	ISteamUser_GetVoice: Function(bWantCompressed:Boolean; pDestBuffer:PByte; cbDestBufferSize:Cardinal; Var nBytesWritten:Cardinal; bWantUncompressed:Boolean; pUncompressedDestBuffer:PByte; cbUncompressedDestBufferSize:Cardinal; Var nUncompressBytesWritten:Cardinal; nUncompressedVoiceDesiredSampleRate:Cardinal):SteamVoiceResult; CDecl;
	ISteamUser_DecompressVoice: Function(pCompressed:PByte; cbCompressed:Cardinal; pDestBuffer:PByte; cbDestBufferSize:Cardinal; Var nBytesWritten:Cardinal; nDesiredSampleRate:Cardinal):SteamVoiceResult; CDecl;
	ISteamUser_GetVoiceOptimalSampleRate: Function():Cardinal; CDecl;
	ISteamUser_GetAuthSessionTicket: Function(pTicket:PByte; cbMaxTicket:Integer; Var pcbTicket:Cardinal):Cardinal; CDecl;
	ISteamUser_BeginAuthSession: Function(pAuthTicket:PByte; cbAuthTicket:Integer; steamID:SteamID):SteamBeginAuthSessionResult; CDecl;
	ISteamUser_EndAuthSession: Procedure(steamID:SteamID); CDecl;
	ISteamUser_CancelAuthTicket: Procedure(hAuthTicket:SteamAuthTicketHandle); CDecl;
	ISteamUser_UserHasLicenseForApp: Function(steamID:SteamID; appID:SteamAppId):SteamUserHasLicenseForAppResult; CDecl;
	ISteamUser_BIsBehindNAT: Function():Boolean; CDecl;
	ISteamUser_AdvertiseGame: Procedure(steamIDGameServer:SteamID; unIPServer:Cardinal; usPortServer:Word); CDecl;
	ISteamUser_RequestEncryptedAppTicket: Function(pDataToInclude:PByte; cbDataToInclude:Integer):UInt64; CDecl;
	ISteamUser_GetEncryptedAppTicket: Function(pTicket:PByte; cbMaxTicket:Integer; Var pcbTicket:Cardinal):Boolean; CDecl;
	ISteamUser_GetGameBadgeLevel: Function(nSeries:Integer; bFoil:Boolean):Integer; CDecl;
	ISteamUser_GetPlayerSteamLevel: Function():Integer; CDecl;

// SteamUserStats
Var
	ISteamUserStats_RequestCurrentStats: Function():Boolean; CDecl;
	ISteamUserStats_GetStat: Function(pchName:PAnsiChar; Var pData:Integer):Boolean; CDecl;
	ISteamUserStats_GetStat_: Function(pchName:PAnsiChar; Var pData:Single):Boolean; CDecl;
	ISteamUserStats_SetStat: Function(pchName:PAnsiChar; nData:Integer):Boolean; CDecl;
	ISteamUserStats_SetStat_: Function(pchName:PAnsiChar; fData:Single):Boolean; CDecl;
	ISteamUserStats_UpdateAvgRateStat: Function(pchName:PAnsiChar; flCountThisSession:Single; dSessionLength:double):Boolean; CDecl;
	ISteamUserStats_GetAchievement: Function(pchName:PAnsiChar; Var pbAchieved:Boolean):Boolean; CDecl;
	ISteamUserStats_SetAchievement: Function(pchName:PAnsiChar):Boolean; CDecl;
	ISteamUserStats_ClearAchievement: Function(pchName:PAnsiChar):Boolean; CDecl;
	ISteamUserStats_GetAchievementAndUnlockTime: Function(pchName:PAnsiChar; Var pbAchieved:Boolean; Var punUnlockTime:Cardinal):Boolean; CDecl;
	ISteamUserStats_StoreStats: Function():Boolean; CDecl;
	ISteamUserStats_GetAchievementIcon: Function(pchName:PAnsiChar):Integer; CDecl;
	ISteamUserStats_GetAchievementDisplayAttribute: Function(pchName:PAnsiChar; pchKey:PAnsiChar):PAnsiChar; CDecl;
	ISteamUserStats_IndicateAchievementProgress: Function(pchName:PAnsiChar; nCurProgress:Cardinal; nMaxProgress:Cardinal):Boolean; CDecl;
	ISteamUserStats_GetNumAchievements: Function():Cardinal; CDecl;
	ISteamUserStats_GetAchievementName: Function(iAchievement:Cardinal):PAnsiChar; CDecl;
	ISteamUserStats_RequestUserStats: Function(steamIDUser:SteamID):UInt64; CDecl;
	ISteamUserStats_GetUserStat: Function(steamIDUser:SteamID; pchName:PAnsiChar; Var pData:Integer):Boolean; CDecl;
	ISteamUserStats_GetUserStat_: Function(steamIDUser:SteamID; pchName:PAnsiChar; Var pData:Single):Boolean; CDecl;
	ISteamUserStats_GetUserAchievement: Function(steamIDUser:SteamID; pchName:PAnsiChar; Var pbAchieved:Boolean):Boolean; CDecl;
	ISteamUserStats_GetUserAchievementAndUnlockTime: Function(steamIDUser:SteamID; pchName:PAnsiChar; Var pbAchieved:Boolean; Var punUnlockTime:Cardinal):Boolean; CDecl;
	ISteamUserStats_ResetAllStats: Function(bAchievementsToo:Boolean):Boolean; CDecl;
	ISteamUserStats_FindOrCreateLeaderboard: Function(pchLeaderboardName:PAnsiChar; eLeaderboardSortMethod:SteamLeaderboardSortMethod; eLeaderboardDisplayType:SteamLeaderboardDisplayType):UInt64; CDecl;
	ISteamUserStats_FindLeaderboard: Function(pchLeaderboardName:PAnsiChar):UInt64; CDecl;
	ISteamUserStats_GetLeaderboardName: Function(hSteamLeaderboard:SteamLeaderboard):PAnsiChar; CDecl;
	ISteamUserStats_GetLeaderboardEntryCount: Function(hSteamLeaderboard:SteamLeaderboard):Integer; CDecl;
	ISteamUserStats_GetLeaderboardSortMethod: Function(hSteamLeaderboard:SteamLeaderboard):SteamLeaderboardSortMethod; CDecl;
	ISteamUserStats_GetLeaderboardDisplayType: Function(hSteamLeaderboard:SteamLeaderboard):SteamLeaderboardDisplayType; CDecl;
	ISteamUserStats_DownloadLeaderboardEntries: Function(hSteamLeaderboard:SteamLeaderboard; eLeaderboardDataRequest:SteamLeaderboardDataRequest; nRangeStart:Integer; nRangeEnd:Integer):UInt64; CDecl;
	ISteamUserStats_DownloadLeaderboardEntriesForUsers: Function(hSteamLeaderboard:SteamLeaderboard; prgUsers:PSteamID; cUsers:Integer):UInt64; CDecl;
	ISteamUserStats_GetDownloadedLeaderboardEntry: Function(hSteamLeaderboardEntries:SteamLeaderboardEntries; index:Integer; Var pLeaderboardEntry:SteamLeaderboardEntry; pDetails:PInteger; cDetailsMax:Integer):Boolean; CDecl;
	ISteamUserStats_UploadLeaderboardScore: Function(hSteamLeaderboard:SteamLeaderboard; eLeaderboardUploadScoreMethod:SteamLeaderboardUploadScoreMethod; nScore:Integer; pScoreDetails:PInteger; cScoreDetailsCount:Integer):UInt64; CDecl;
	ISteamUserStats_AttachLeaderboardUGC: Function(hSteamLeaderboard:SteamLeaderboard; hUGC:SteamUGCHandle):UInt64; CDecl;
	ISteamUserStats_GetNumberOfCurrentPlayers: Function():UInt64; CDecl;
	ISteamUserStats_RequestGlobalAchievementPercentages: Function():UInt64; CDecl;
	ISteamUserStats_GetMostAchievedAchievementInfo: Function(pchName:Pointer; unNameBufLen:Cardinal; Var pflPercent:Single; Var pbAchieved:Boolean):Integer; CDecl;
	ISteamUserStats_GetNextMostAchievedAchievementInfo: Function(iIteratorPrevious:Integer; pchName:Pointer; unNameBufLen:Cardinal; Var pflPercent:Single; Var pbAchieved:Boolean):Integer; CDecl;
	ISteamUserStats_GetAchievementAchievedPercent: Function(pchName:PAnsiChar; Var pflPercent:Single):Boolean; CDecl;
	ISteamUserStats_RequestGlobalStats: Function(nHistoryDays:Integer):UInt64; CDecl;
	ISteamUserStats_GetGlobalStat: Function(pchStatName:PAnsiChar; Var pData:Int64):Boolean; CDecl;
	ISteamUserStats_GetGlobalStat_: Function(pchStatName:PAnsiChar; Var pData:double):Boolean; CDecl;
	ISteamUserStats_GetGlobalStatHistory: Function(pchStatName:PAnsiChar; pData:PInt64; cubData:Cardinal):Integer; CDecl;
	ISteamUserStats_GetGlobalStatHistory_: Function(pchStatName:PAnsiChar; pData:Pdouble; cubData:Cardinal):Integer; CDecl;

// SteamUtils
Var
	ISteamUtils_GetSecondsSinceAppActive: Function():Cardinal; CDecl;
	ISteamUtils_GetSecondsSinceComputerActive: Function():Cardinal; CDecl;
	ISteamUtils_GetConnectedUniverse: Function():SteamUniverse; CDecl;
	ISteamUtils_GetServerRealTime: Function():Cardinal; CDecl;
	ISteamUtils_GetIPCountry: Function():PAnsiChar; CDecl;
	ISteamUtils_GetImageSize: Function(iImage:Integer; Var pnWidth:Cardinal; Var pnHeight:Cardinal):Boolean; CDecl;
	ISteamUtils_GetImageRGBA: Function(iImage:Integer; pubDest:PByte; nDestBufferSize:Integer):Boolean; CDecl;
	ISteamUtils_GetCSERIPPort: Function(Var unIP:Cardinal; Var usPort:Word):Boolean; CDecl;
	ISteamUtils_GetCurrentBatteryPower: Function():Byte; CDecl;
	ISteamUtils_GetAppID: Function():Cardinal; CDecl;
	ISteamUtils_SetOverlayNotificationPosition: Procedure(eNotificationPosition:SteamNotificationPosition); CDecl;
	ISteamUtils_IsAPICallCompleted: Function(hSteamAPICall:SteamAPICall; Var pbFailed:Boolean):Boolean; CDecl;
	ISteamUtils_GetAPICallFailureReason: Function(hSteamAPICall:SteamAPICall):SteamAPICallFailure; CDecl;
	ISteamUtils_GetAPICallResult: Function(hSteamAPICall:SteamAPICall; pCallback:Pointer; cubCallback:Integer; iCallbackExpected:Integer; Var pbFailed:Boolean):Boolean; CDecl;
	ISteamUtils_RunFrame: Procedure(); CDecl;
	ISteamUtils_GetIPCCallCount: Function():Cardinal; CDecl;
	ISteamUtils_SetWarningMessageHook: Procedure(pFunction:SteamAPIWarningMessageHook); CDecl;
	ISteamUtils_IsOverlayEnabled: Function():Boolean; CDecl;
	ISteamUtils_BOverlayNeedsPresent: Function():Boolean; CDecl;
	ISteamUtils_ShowGamepadTextInput: Function(eInputMode:SteamGamepadTextInputMode; eLineInputMode:SteamGamepadTextInputLineMode; pchDescription:PAnsiChar; unCharMax:Cardinal; pchExistingText:PAnsiChar):Boolean; CDecl;
	ISteamUtils_GetEnteredGamepadTextLength: Function():Cardinal; CDecl;
	ISteamUtils_GetEnteredGamepadTextInput: Function(pchText:Pointer; cchText:Cardinal):Boolean; CDecl;
	ISteamUtils_GetSteamUILanguage: Function():PAnsiChar; CDecl;
	ISteamUtils_IsSteamRunningInVR: Function():Boolean; CDecl;

Function LoadSteamAPI():Boolean;


Implementation

Var
  SteamHandle:{$IFDEF MSWINDOWS}THandle{$ELSE}TLibHandle{$ENDIF};

Function LoadSteamAPI():Boolean; 
Begin
  If SteamHandle<>0 Then
  Begin
    Result := True;
    Exit;
  End;

  SteamHandle := LoadLibrary(SteamWrapperName);
  If SteamHandle=0 Then
  Begin
    Result := False;
    Exit;
   End;

	SteamAPI_Shutdown := GetProcAddress(SteamHandle, 'Shutdown');
	SteamAPI_IsSteamRunning := GetProcAddress(SteamHandle, 'IsSteamRunning');
	SteamAPI_RestartAppIfNecessary := GetProcAddress(SteamHandle, 'RestartAppIfNecessary');
	SteamAPI_WriteMiniDump := GetProcAddress(SteamHandle, 'WriteMiniDump');
	SteamAPI_SetMiniDumpComment := GetProcAddress(SteamHandle, 'SetMiniDumpComment');
	SteamClient := GetProcAddress(SteamHandle, 'SteamClient_');
	SteamAPI_InitSafe := GetProcAddress(SteamHandle, 'InitSafe');
	SteamAPI_RunCallbacks := GetProcAddress(SteamHandle, 'RunCallbacks');
	SteamAPI_RegisterCallback := GetProcAddress(SteamHandle, 'RegisterCallback');
	SteamAPI_UnregisterCallback := GetProcAddress(SteamHandle, 'UnregisterCallback');
	SteamAPI_RegisterCallResult := GetProcAddress(SteamHandle, 'RegisterCallResult');
	SteamAPI_UnregisterCallResult := GetProcAddress(SteamHandle, 'UnregisterCallResult');
	Steam_RunCallbacks := GetProcAddress(SteamHandle, 'Steam_RunCallbacks_');
	Steam_RegisterInterfaceFuncs := GetProcAddress(SteamHandle, 'Steam_RegisterInterfaceFuncs_');
	Steam_GetHSteamUserCurrent := GetProcAddress(SteamHandle, 'Steam_GetHSteamUserCurrent_');
	SteamAPI_GetSteamInstallPath := GetProcAddress(SteamHandle, 'GetSteamInstallPath');
	SteamAPI_GetHSteamPipe := GetProcAddress(SteamHandle, 'GetHSteamPipe_');
	SteamAPI_SetTryCatchCallbacks := GetProcAddress(SteamHandle, 'SetTryCatchCallbacks');
	SteamAPI_GetHSteamUser := GetProcAddress(SteamHandle, 'GetHSteamUser_');
	SteamAPI_UseBreakpadCrashHandler := GetProcAddress(SteamHandle, 'UseBreakpadCrashHandler');
	SteamUser := GetProcAddress(SteamHandle, 'SteamUser');
	SteamFriends := GetProcAddress(SteamHandle, 'SteamFriends');
	SteamUtils := GetProcAddress(SteamHandle, 'SteamUtils');
	SteamMatchmaking := GetProcAddress(SteamHandle, 'SteamMatchmaking');
	SteamUserStats := GetProcAddress(SteamHandle, 'SteamUserStats');
	SteamApps := GetProcAddress(SteamHandle, 'SteamApps');
	SteamNetworking := GetProcAddress(SteamHandle, 'SteamNetworking');
	SteamMatchmakingServers := GetProcAddress(SteamHandle, 'SteamMatchmakingServers');
	SteamRemoteStorage := GetProcAddress(SteamHandle, 'SteamRemoteStorage');
	SteamScreenshots := GetProcAddress(SteamHandle, 'SteamScreenshots');
	SteamHTTP := GetProcAddress(SteamHandle, 'SteamHTTP');
	SteamUnifiedMessages := GetProcAddress(SteamHandle, 'SteamUnifiedMessages');
	SteamController := GetProcAddress(SteamHandle, 'SteamController');
	SteamUGC := GetProcAddress(SteamHandle, 'SteamUGC');
	SteamAppList := GetProcAddress(SteamHandle, 'SteamAppList');
	SteamMusic := GetProcAddress(SteamHandle, 'SteamMusic');
	SteamMusicRemote := GetProcAddress(SteamHandle, 'SteamMusicRemote');
	SteamGameServer_InitSafe := GetProcAddress(SteamHandle, 'GameServer_InitSafe');
	SteamGameServer_Shutdown := GetProcAddress(SteamHandle, 'GameServer_Shutdown');
	SteamGameServer_RunCallbacks := GetProcAddress(SteamHandle, 'GameServer_RunCallbacks');
	SteamGameServer_BSecure := GetProcAddress(SteamHandle, 'GameServer_BSecure');
	SteamGameServer_GetSteamID := GetProcAddress(SteamHandle, 'GameServer_GetSteamID');
	SteamGameServer_GetHSteamPipe := GetProcAddress(SteamHandle, 'GameServer_GetHSteamPipe');
	SteamGameServer_GetHSteamUser := GetProcAddress(SteamHandle, 'GameServer_GetHSteamUser');
	SteamClientGameServer := GetProcAddress(SteamHandle, 'SteamClientGameServer');
	SteamGameServer := GetProcAddress(SteamHandle, 'SteamGameServer');
	SteamGameServerUtils := GetProcAddress(SteamHandle, 'SteamGameServerUtils');
	SteamGameServerNetworking := GetProcAddress(SteamHandle, 'SteamGameServerNetworking');
	SteamGameServerStats := GetProcAddress(SteamHandle, 'SteamGameServerStats');
	SteamGameServerHTTP := GetProcAddress(SteamHandle, 'SteamGameServerHTTP');
	BDecryptTicket := GetProcAddress(SteamHandle, 'SteamEncryptedAppTicket_BDecryptTicket');
	BIsTicketForApp := GetProcAddress(SteamHandle, 'SteamEncryptedAppTicket_BIsTicketForApp');
	GetTicketIssueTime := GetProcAddress(SteamHandle, 'SteamEncryptedAppTicket_GetTicketIssueTime');
	GetTicketSteamID := GetProcAddress(SteamHandle, 'SteamEncryptedAppTicket_GetTicketSteamID');
	GetTicketAppID := GetProcAddress(SteamHandle, 'SteamEncryptedAppTicket_GetTicketAppID');
	BUserOwnsAppInTicket := GetProcAddress(SteamHandle, 'SteamEncryptedAppTicket_BUserOwnsAppInTicket');
	BUserIsVacBanned := GetProcAddress(SteamHandle, 'SteamEncryptedAppTicket_BUserIsVacBanned');
	GetUserVariableData := GetProcAddress(SteamHandle, 'SteamEncryptedAppTicket_GetUserVariableData');
	ISteamAppList_GetNumInstalledApps := GetProcAddress(SteamHandle, 'ISteamAppList_GetNumInstalledApps');
	ISteamAppList_GetInstalledApps := GetProcAddress(SteamHandle, 'ISteamAppList_GetInstalledApps');
	ISteamAppList_GetAppName := GetProcAddress(SteamHandle, 'ISteamAppList_GetAppName');
	ISteamAppList_GetAppInstallDir := GetProcAddress(SteamHandle, 'ISteamAppList_GetAppInstallDir');
	ISteamAppList_GetAppBuildId := GetProcAddress(SteamHandle, 'ISteamAppList_GetAppBuildId');
	ISteamApps_BIsSubscribed := GetProcAddress(SteamHandle, 'ISteamApps_BIsSubscribed');
	ISteamApps_BIsLowViolence := GetProcAddress(SteamHandle, 'ISteamApps_BIsLowViolence');
	ISteamApps_BIsCybercafe := GetProcAddress(SteamHandle, 'ISteamApps_BIsCybercafe');
	ISteamApps_BIsVACBanned := GetProcAddress(SteamHandle, 'ISteamApps_BIsVACBanned');
	ISteamApps_GetCurrentGameLanguage := GetProcAddress(SteamHandle, 'ISteamApps_GetCurrentGameLanguage');
	ISteamApps_GetAvailableGameLanguages := GetProcAddress(SteamHandle, 'ISteamApps_GetAvailableGameLanguages');
	ISteamApps_BIsSubscribedApp := GetProcAddress(SteamHandle, 'ISteamApps_BIsSubscribedApp');
	ISteamApps_BIsDlcInstalled := GetProcAddress(SteamHandle, 'ISteamApps_BIsDlcInstalled');
	ISteamApps_GetEarliestPurchaseUnixTime := GetProcAddress(SteamHandle, 'ISteamApps_GetEarliestPurchaseUnixTime');
	ISteamApps_BIsSubscribedFromFreeWeekend := GetProcAddress(SteamHandle, 'ISteamApps_BIsSubscribedFromFreeWeekend');
	ISteamApps_GetDLCCount := GetProcAddress(SteamHandle, 'ISteamApps_GetDLCCount');
	ISteamApps_BGetDLCDataByIndex := GetProcAddress(SteamHandle, 'ISteamApps_BGetDLCDataByIndex');
	ISteamApps_InstallDLC := GetProcAddress(SteamHandle, 'ISteamApps_InstallDLC');
	ISteamApps_UninstallDLC := GetProcAddress(SteamHandle, 'ISteamApps_UninstallDLC');
	ISteamApps_RequestAppProofOfPurchaseKey := GetProcAddress(SteamHandle, 'ISteamApps_RequestAppProofOfPurchaseKey');
	ISteamApps_GetCurrentBetaName := GetProcAddress(SteamHandle, 'ISteamApps_GetCurrentBetaName');
	ISteamApps_MarkContentCorrupt := GetProcAddress(SteamHandle, 'ISteamApps_MarkContentCorrupt');
	ISteamApps_GetInstalledDepots := GetProcAddress(SteamHandle, 'ISteamApps_GetInstalledDepots');
	ISteamApps_GetAppInstallDir := GetProcAddress(SteamHandle, 'ISteamApps_GetAppInstallDir');
	ISteamApps_BIsAppInstalled := GetProcAddress(SteamHandle, 'ISteamApps_BIsAppInstalled');
	ISteamApps_GetAppOwner := GetProcAddress(SteamHandle, 'ISteamApps_GetAppOwner');
	ISteamApps_GetLaunchQueryParam := GetProcAddress(SteamHandle, 'ISteamApps_GetLaunchQueryParam');
	ISteamClient_CreateSteamPipe := GetProcAddress(SteamHandle, 'ISteamClient_CreateSteamPipe');
	ISteamClient_BReleaseSteamPipe := GetProcAddress(SteamHandle, 'ISteamClient_BReleaseSteamPipe');
	ISteamClient_ConnectToGlobalUser := GetProcAddress(SteamHandle, 'ISteamClient_ConnectToGlobalUser');
	ISteamClient_CreateLocalUser := GetProcAddress(SteamHandle, 'ISteamClient_CreateLocalUser');
	ISteamClient_ReleaseUser := GetProcAddress(SteamHandle, 'ISteamClient_ReleaseUser');
	ISteamClient_GetISteamUser := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamUser');
	ISteamClient_GetISteamGameServer := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamGameServer');
	ISteamClient_SetLocalIPBinding := GetProcAddress(SteamHandle, 'ISteamClient_SetLocalIPBinding');
	ISteamClient_GetISteamFriends := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamFriends');
	ISteamClient_GetISteamUtils := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamUtils');
	ISteamClient_GetISteamMatchmaking := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamMatchmaking');
	ISteamClient_GetISteamMatchmakingServers := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamMatchmakingServers');
	ISteamClient_GetISteamGenericInterface := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamGenericInterface');
	ISteamClient_GetISteamUserStats := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamUserStats');
	ISteamClient_GetISteamGameServerStats := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamGameServerStats');
	ISteamClient_GetISteamApps := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamApps');
	ISteamClient_GetISteamNetworking := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamNetworking');
	ISteamClient_GetISteamRemoteStorage := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamRemoteStorage');
	ISteamClient_GetISteamScreenshots := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamScreenshots');
	ISteamClient_RunFrame := GetProcAddress(SteamHandle, 'ISteamClient_RunFrame');
	ISteamClient_GetIPCCallCount := GetProcAddress(SteamHandle, 'ISteamClient_GetIPCCallCount');
	ISteamClient_SetWarningMessageHook := GetProcAddress(SteamHandle, 'ISteamClient_SetWarningMessageHook');
	ISteamClient_BShutdownIfAllPipesClosed := GetProcAddress(SteamHandle, 'ISteamClient_BShutdownIfAllPipesClosed');
	ISteamClient_GetISteamHTTP := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamHTTP');
	ISteamClient_GetISteamUnifiedMessages := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamUnifiedMessages');
	ISteamClient_GetISteamController := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamController');
	ISteamClient_GetISteamUGC := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamUGC');
	ISteamClient_GetISteamAppList := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamAppList');
	ISteamClient_GetISteamMusic := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamMusic');
	ISteamClient_GetISteamMusicRemote := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamMusicRemote');
	ISteamClient_GetISteamHTMLSurface := GetProcAddress(SteamHandle, 'ISteamClient_GetISteamHTMLSurface');
	ISteamClient_Set_SteamAPI_CPostAPIResultInProcess := GetProcAddress(SteamHandle, 'ISteamClient_Set_SteamAPI_CPostAPIResultInProcess');
	ISteamClient_Remove_SteamAPI_CPostAPIResultInProcess := GetProcAddress(SteamHandle, 'ISteamClient_Remove_SteamAPI_CPostAPIResultInProcess');
	ISteamClient_Set_SteamAPI_CCheckCallbackRegisteredInProcess := GetProcAddress(SteamHandle, 'ISteamClient_Set_SteamAPI_CCheckCallbackRegisteredInProcess');
	ISteamController_Init := GetProcAddress(SteamHandle, 'ISteamController_Init');
	ISteamController_Shutdown := GetProcAddress(SteamHandle, 'ISteamController_Shutdown');
	ISteamController_RunFrame := GetProcAddress(SteamHandle, 'ISteamController_RunFrame');
	ISteamController_GetControllerState := GetProcAddress(SteamHandle, 'ISteamController_GetControllerState');
	ISteamController_TriggerHapticPulse := GetProcAddress(SteamHandle, 'ISteamController_TriggerHapticPulse');
	ISteamController_SetOverrideMode := GetProcAddress(SteamHandle, 'ISteamController_SetOverrideMode');
	ISteamFriends_GetPersonaName := GetProcAddress(SteamHandle, 'ISteamFriends_GetPersonaName');
	ISteamFriends_SetPersonaName := GetProcAddress(SteamHandle, 'ISteamFriends_SetPersonaName');
	ISteamFriends_GetPersonaState := GetProcAddress(SteamHandle, 'ISteamFriends_GetPersonaState');
	ISteamFriends_GetFriendCount := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendCount');
	ISteamFriends_GetFriendByIndex := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendByIndex');
	ISteamFriends_GetFriendRelationship := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendRelationship');
	ISteamFriends_GetFriendPersonaState := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendPersonaState');
	ISteamFriends_GetFriendPersonaName := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendPersonaName');
	ISteamFriends_GetFriendGamePlayed := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendGamePlayed');
	ISteamFriends_GetFriendPersonaNameHistory := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendPersonaNameHistory');
	ISteamFriends_GetPlayerNickname := GetProcAddress(SteamHandle, 'ISteamFriends_GetPlayerNickname');
	ISteamFriends_HasFriend := GetProcAddress(SteamHandle, 'ISteamFriends_HasFriend');
	ISteamFriends_GetClanCount := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanCount');
	ISteamFriends_GetClanByIndex := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanByIndex');
	ISteamFriends_GetClanName := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanName');
	ISteamFriends_GetClanTag := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanTag');
	ISteamFriends_GetClanActivityCounts := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanActivityCounts');
	ISteamFriends_DownloadClanActivityCounts := GetProcAddress(SteamHandle, 'ISteamFriends_DownloadClanActivityCounts');
	ISteamFriends_GetFriendCountFromSource := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendCountFromSource');
	ISteamFriends_GetFriendFromSourceByIndex := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendFromSourceByIndex');
	ISteamFriends_IsUserInSource := GetProcAddress(SteamHandle, 'ISteamFriends_IsUserInSource');
	ISteamFriends_SetInGameVoiceSpeaking := GetProcAddress(SteamHandle, 'ISteamFriends_SetInGameVoiceSpeaking');
	ISteamFriends_ActivateGameOverlay := GetProcAddress(SteamHandle, 'ISteamFriends_ActivateGameOverlay');
	ISteamFriends_ActivateGameOverlayToUser := GetProcAddress(SteamHandle, 'ISteamFriends_ActivateGameOverlayToUser');
	ISteamFriends_ActivateGameOverlayToWebPage := GetProcAddress(SteamHandle, 'ISteamFriends_ActivateGameOverlayToWebPage');
	ISteamFriends_ActivateGameOverlayToStore := GetProcAddress(SteamHandle, 'ISteamFriends_ActivateGameOverlayToStore');
	ISteamFriends_SetPlayedWith := GetProcAddress(SteamHandle, 'ISteamFriends_SetPlayedWith');
	ISteamFriends_ActivateGameOverlayInviteDialog := GetProcAddress(SteamHandle, 'ISteamFriends_ActivateGameOverlayInviteDialog');
	ISteamFriends_GetSmallFriendAvatar := GetProcAddress(SteamHandle, 'ISteamFriends_GetSmallFriendAvatar');
	ISteamFriends_GetMediumFriendAvatar := GetProcAddress(SteamHandle, 'ISteamFriends_GetMediumFriendAvatar');
	ISteamFriends_GetLargeFriendAvatar := GetProcAddress(SteamHandle, 'ISteamFriends_GetLargeFriendAvatar');
	ISteamFriends_RequestUserInformation := GetProcAddress(SteamHandle, 'ISteamFriends_RequestUserInformation');
	ISteamFriends_RequestClanOfficerList := GetProcAddress(SteamHandle, 'ISteamFriends_RequestClanOfficerList');
	ISteamFriends_GetClanOwner := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanOwner');
	ISteamFriends_GetClanOfficerCount := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanOfficerCount');
	ISteamFriends_GetClanOfficerByIndex := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanOfficerByIndex');
	ISteamFriends_GetUserRestrictions := GetProcAddress(SteamHandle, 'ISteamFriends_GetUserRestrictions');
	ISteamFriends_SetRichPresence := GetProcAddress(SteamHandle, 'ISteamFriends_SetRichPresence');
	ISteamFriends_ClearRichPresence := GetProcAddress(SteamHandle, 'ISteamFriends_ClearRichPresence');
	ISteamFriends_GetFriendRichPresence := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendRichPresence');
	ISteamFriends_GetFriendRichPresenceKeyCount := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendRichPresenceKeyCount');
	ISteamFriends_GetFriendRichPresenceKeyByIndex := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendRichPresenceKeyByIndex');
	ISteamFriends_RequestFriendRichPresence := GetProcAddress(SteamHandle, 'ISteamFriends_RequestFriendRichPresence');
	ISteamFriends_InviteUserToGame := GetProcAddress(SteamHandle, 'ISteamFriends_InviteUserToGame');
	ISteamFriends_GetCoplayFriendCount := GetProcAddress(SteamHandle, 'ISteamFriends_GetCoplayFriendCount');
	ISteamFriends_GetCoplayFriend := GetProcAddress(SteamHandle, 'ISteamFriends_GetCoplayFriend');
	ISteamFriends_GetFriendCoplayTime := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendCoplayTime');
	ISteamFriends_GetFriendCoplayGame := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendCoplayGame');
	ISteamFriends_JoinClanChatRoom := GetProcAddress(SteamHandle, 'ISteamFriends_JoinClanChatRoom');
	ISteamFriends_LeaveClanChatRoom := GetProcAddress(SteamHandle, 'ISteamFriends_LeaveClanChatRoom');
	ISteamFriends_GetClanChatMemberCount := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanChatMemberCount');
	ISteamFriends_GetChatMemberByIndex := GetProcAddress(SteamHandle, 'ISteamFriends_GetChatMemberByIndex');
	ISteamFriends_SendClanChatMessage := GetProcAddress(SteamHandle, 'ISteamFriends_SendClanChatMessage');
	ISteamFriends_GetClanChatMessage := GetProcAddress(SteamHandle, 'ISteamFriends_GetClanChatMessage');
	ISteamFriends_IsClanChatAdmin := GetProcAddress(SteamHandle, 'ISteamFriends_IsClanChatAdmin');
	ISteamFriends_IsClanChatWindowOpenInSteam := GetProcAddress(SteamHandle, 'ISteamFriends_IsClanChatWindowOpenInSteam');
	ISteamFriends_OpenClanChatWindowInSteam := GetProcAddress(SteamHandle, 'ISteamFriends_OpenClanChatWindowInSteam');
	ISteamFriends_CloseClanChatWindowInSteam := GetProcAddress(SteamHandle, 'ISteamFriends_CloseClanChatWindowInSteam');
	ISteamFriends_SetListenForFriendsMessages := GetProcAddress(SteamHandle, 'ISteamFriends_SetListenForFriendsMessages');
	ISteamFriends_ReplyToFriendMessage := GetProcAddress(SteamHandle, 'ISteamFriends_ReplyToFriendMessage');
	ISteamFriends_GetFriendMessage := GetProcAddress(SteamHandle, 'ISteamFriends_GetFriendMessage');
	ISteamFriends_GetFollowerCount := GetProcAddress(SteamHandle, 'ISteamFriends_GetFollowerCount');
	ISteamFriends_IsFollowing := GetProcAddress(SteamHandle, 'ISteamFriends_IsFollowing');
	ISteamFriends_EnumerateFollowingList := GetProcAddress(SteamHandle, 'ISteamFriends_EnumerateFollowingList');
	ISteamGameServer_InitGameServer := GetProcAddress(SteamHandle, 'ISteamGameServer_InitGameServer');
	ISteamGameServer_SetProduct := GetProcAddress(SteamHandle, 'ISteamGameServer_SetProduct');
	ISteamGameServer_SetGameDescription := GetProcAddress(SteamHandle, 'ISteamGameServer_SetGameDescription');
	ISteamGameServer_SetModDir := GetProcAddress(SteamHandle, 'ISteamGameServer_SetModDir');
	ISteamGameServer_SetDedicatedServer := GetProcAddress(SteamHandle, 'ISteamGameServer_SetDedicatedServer');
	ISteamGameServer_LogOn := GetProcAddress(SteamHandle, 'ISteamGameServer_LogOn');
	ISteamGameServer_LogOnAnonymous := GetProcAddress(SteamHandle, 'ISteamGameServer_LogOnAnonymous');
	ISteamGameServer_LogOff := GetProcAddress(SteamHandle, 'ISteamGameServer_LogOff');
	ISteamGameServer_BLoggedOn := GetProcAddress(SteamHandle, 'ISteamGameServer_BLoggedOn');
	ISteamGameServer_BSecure := GetProcAddress(SteamHandle, 'ISteamGameServer_BSecure');
	ISteamGameServer_GetSteamID := GetProcAddress(SteamHandle, 'ISteamGameServer_GetSteamID');
	ISteamGameServer_WasRestartRequested := GetProcAddress(SteamHandle, 'ISteamGameServer_WasRestartRequested');
	ISteamGameServer_SetMaxPlayerCount := GetProcAddress(SteamHandle, 'ISteamGameServer_SetMaxPlayerCount');
	ISteamGameServer_SetBotPlayerCount := GetProcAddress(SteamHandle, 'ISteamGameServer_SetBotPlayerCount');
	ISteamGameServer_SetServerName := GetProcAddress(SteamHandle, 'ISteamGameServer_SetServerName');
	ISteamGameServer_SetMapName := GetProcAddress(SteamHandle, 'ISteamGameServer_SetMapName');
	ISteamGameServer_SetPasswordProtected := GetProcAddress(SteamHandle, 'ISteamGameServer_SetPasswordProtected');
	ISteamGameServer_SetSpectatorPort := GetProcAddress(SteamHandle, 'ISteamGameServer_SetSpectatorPort');
	ISteamGameServer_SetSpectatorServerName := GetProcAddress(SteamHandle, 'ISteamGameServer_SetSpectatorServerName');
	ISteamGameServer_ClearAllKeyValues := GetProcAddress(SteamHandle, 'ISteamGameServer_ClearAllKeyValues');
	ISteamGameServer_SetKeyValue := GetProcAddress(SteamHandle, 'ISteamGameServer_SetKeyValue');
	ISteamGameServer_SetGameTags := GetProcAddress(SteamHandle, 'ISteamGameServer_SetGameTags');
	ISteamGameServer_SetGameData := GetProcAddress(SteamHandle, 'ISteamGameServer_SetGameData');
	ISteamGameServer_SetRegion := GetProcAddress(SteamHandle, 'ISteamGameServer_SetRegion');
	ISteamGameServer_SendUserConnectAndAuthenticate := GetProcAddress(SteamHandle, 'ISteamGameServer_SendUserConnectAndAuthenticate');
	ISteamGameServer_CreateUnauthenticatedUserConnection := GetProcAddress(SteamHandle, 'ISteamGameServer_CreateUnauthenticatedUserConnection');
	ISteamGameServer_SendUserDisconnect := GetProcAddress(SteamHandle, 'ISteamGameServer_SendUserDisconnect');
	ISteamGameServer_BUpdateUserData := GetProcAddress(SteamHandle, 'ISteamGameServer_BUpdateUserData');
	ISteamGameServer_GetAuthSessionTicket := GetProcAddress(SteamHandle, 'ISteamGameServer_GetAuthSessionTicket');
	ISteamGameServer_BeginAuthSession := GetProcAddress(SteamHandle, 'ISteamGameServer_BeginAuthSession');
	ISteamGameServer_EndAuthSession := GetProcAddress(SteamHandle, 'ISteamGameServer_EndAuthSession');
	ISteamGameServer_CancelAuthTicket := GetProcAddress(SteamHandle, 'ISteamGameServer_CancelAuthTicket');
	ISteamGameServer_UserHasLicenseForApp := GetProcAddress(SteamHandle, 'ISteamGameServer_UserHasLicenseForApp');
	ISteamGameServer_RequestUserGroupStatus := GetProcAddress(SteamHandle, 'ISteamGameServer_RequestUserGroupStatus');
	ISteamGameServer_GetGameplayStats := GetProcAddress(SteamHandle, 'ISteamGameServer_GetGameplayStats');
	ISteamGameServer_GetServerReputation := GetProcAddress(SteamHandle, 'ISteamGameServer_GetServerReputation');
	ISteamGameServer_GetPublicIP := GetProcAddress(SteamHandle, 'ISteamGameServer_GetPublicIP');
	ISteamGameServer_HandleIncomingPacket := GetProcAddress(SteamHandle, 'ISteamGameServer_HandleIncomingPacket');
	ISteamGameServer_GetNextOutgoingPacket := GetProcAddress(SteamHandle, 'ISteamGameServer_GetNextOutgoingPacket');
	ISteamGameServer_EnableHeartbeats := GetProcAddress(SteamHandle, 'ISteamGameServer_EnableHeartbeats');
	ISteamGameServer_SetHeartbeatInterval := GetProcAddress(SteamHandle, 'ISteamGameServer_SetHeartbeatInterval');
	ISteamGameServer_ForceHeartbeat := GetProcAddress(SteamHandle, 'ISteamGameServer_ForceHeartbeat');
	ISteamGameServer_AssociateWithClan := GetProcAddress(SteamHandle, 'ISteamGameServer_AssociateWithClan');
	ISteamGameServer_ComputeNewPlayerCompatibility := GetProcAddress(SteamHandle, 'ISteamGameServer_ComputeNewPlayerCompatibility');
	ISteamGameServerHTTP_CreateHTTPRequest := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_CreateHTTPRequest');
	ISteamGameServerHTTP_SetHTTPRequestContextValue := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_SetHTTPRequestContextValue');
	ISteamGameServerHTTP_SetHTTPRequestNetworkActivityTimeout := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_SetHTTPRequestNetworkActivityTimeout');
	ISteamGameServerHTTP_SetHTTPRequestHeaderValue := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_SetHTTPRequestHeaderValue');
	ISteamGameServerHTTP_SetHTTPRequestGetOrPostParameter := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_SetHTTPRequestGetOrPostParameter');
	ISteamGameServerHTTP_SendHTTPRequest := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_SendHTTPRequest');
	ISteamGameServerHTTP_SendHTTPRequestAndStreamResponse := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_SendHTTPRequestAndStreamResponse');
	ISteamGameServerHTTP_DeferHTTPRequest := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_DeferHTTPRequest');
	ISteamGameServerHTTP_PrioritizeHTTPRequest := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_PrioritizeHTTPRequest');
	ISteamGameServerHTTP_GetHTTPResponseHeaderSize := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_GetHTTPResponseHeaderSize');
	ISteamGameServerHTTP_GetHTTPResponseHeaderValue := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_GetHTTPResponseHeaderValue');
	ISteamGameServerHTTP_GetHTTPResponseBodySize := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_GetHTTPResponseBodySize');
	ISteamGameServerHTTP_GetHTTPResponseBodyData := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_GetHTTPResponseBodyData');
	ISteamGameServerHTTP_GetHTTPStreamingResponseBodyData := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_GetHTTPStreamingResponseBodyData');
	ISteamGameServerHTTP_ReleaseHTTPRequest := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_ReleaseHTTPRequest');
	ISteamGameServerHTTP_GetHTTPDownloadProgressPct := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_GetHTTPDownloadProgressPct');
	ISteamGameServerHTTP_SetHTTPRequestRawPostBody := GetProcAddress(SteamHandle, 'ISteamGameServerHTTP_SetHTTPRequestRawPostBody');
	ISteamGameServerNetworking_SendP2PPacket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_SendP2PPacket');
	ISteamGameServerNetworking_IsP2PPacketAvailable := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_IsP2PPacketAvailable');
	ISteamGameServerNetworking_ReadP2PPacket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_ReadP2PPacket');
	ISteamGameServerNetworking_AcceptP2PSessionWithUser := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_AcceptP2PSessionWithUser');
	ISteamGameServerNetworking_CloseP2PSessionWithUser := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_CloseP2PSessionWithUser');
	ISteamGameServerNetworking_CloseP2PChannelWithUser := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_CloseP2PChannelWithUser');
	ISteamGameServerNetworking_GetP2PSessionState := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_GetP2PSessionState');
	ISteamGameServerNetworking_AllowP2PPacketRelay := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_AllowP2PPacketRelay');
	ISteamGameServerNetworking_CreateListenSocket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_CreateListenSocket');
	ISteamGameServerNetworking_CreateP2PConnectionSocket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_CreateP2PConnectionSocket');
	ISteamGameServerNetworking_CreateConnectionSocket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_CreateConnectionSocket');
	ISteamGameServerNetworking_DestroySocket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_DestroySocket');
	ISteamGameServerNetworking_DestroyListenSocket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_DestroyListenSocket');
	ISteamGameServerNetworking_SendDataOnSocket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_SendDataOnSocket');
	ISteamGameServerNetworking_IsDataAvailableOnSocket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_IsDataAvailableOnSocket');
	ISteamGameServerNetworking_RetrieveDataFromSocket := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_RetrieveDataFromSocket');
	ISteamGameServerNetworking_IsDataAvailable := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_IsDataAvailable');
	ISteamGameServerNetworking_RetrieveData := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_RetrieveData');
	ISteamGameServerNetworking_GetSocketInfo := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_GetSocketInfo');
	ISteamGameServerNetworking_GetListenSocketInfo := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_GetListenSocketInfo');
	ISteamGameServerNetworking_GetSocketConnectionType := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_GetSocketConnectionType');
	ISteamGameServerNetworking_GetMaxPacketSize := GetProcAddress(SteamHandle, 'ISteamGameServerNetworking_GetMaxPacketSize');
	ISteamGameServerStats_RequestUserStats := GetProcAddress(SteamHandle, 'ISteamGameServerStats_RequestUserStats');
	ISteamGameServerStats_GetUserStat := GetProcAddress(SteamHandle, 'ISteamGameServerStats_GetUserStat');
	ISteamGameServerStats_GetUserStat_ := GetProcAddress(SteamHandle, 'ISteamGameServerStats_GetUserStat_');
	ISteamGameServerStats_GetUserAchievement := GetProcAddress(SteamHandle, 'ISteamGameServerStats_GetUserAchievement');
	ISteamGameServerStats_SetUserStat := GetProcAddress(SteamHandle, 'ISteamGameServerStats_SetUserStat');
	ISteamGameServerStats_SetUserStat_ := GetProcAddress(SteamHandle, 'ISteamGameServerStats_SetUserStat_');
	ISteamGameServerStats_UpdateUserAvgRateStat := GetProcAddress(SteamHandle, 'ISteamGameServerStats_UpdateUserAvgRateStat');
	ISteamGameServerStats_SetUserAchievement := GetProcAddress(SteamHandle, 'ISteamGameServerStats_SetUserAchievement');
	ISteamGameServerStats_ClearUserAchievement := GetProcAddress(SteamHandle, 'ISteamGameServerStats_ClearUserAchievement');
	ISteamGameServerStats_StoreUserStats := GetProcAddress(SteamHandle, 'ISteamGameServerStats_StoreUserStats');
	ISteamGameServerUtils_GetSecondsSinceAppActive := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetSecondsSinceAppActive');
	ISteamGameServerUtils_GetSecondsSinceComputerActive := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetSecondsSinceComputerActive');
	ISteamGameServerUtils_GetConnectedUniverse := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetConnectedUniverse');
	ISteamGameServerUtils_GetServerRealTime := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetServerRealTime');
	ISteamGameServerUtils_GetIPCountry := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetIPCountry');
	ISteamGameServerUtils_GetImageSize := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetImageSize');
	ISteamGameServerUtils_GetImageRGBA := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetImageRGBA');
	ISteamGameServerUtils_GetCSERIPPort := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetCSERIPPort');
	ISteamGameServerUtils_GetCurrentBatteryPower := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetCurrentBatteryPower');
	ISteamGameServerUtils_GetAppID := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetAppID');
	ISteamGameServerUtils_SetOverlayNotificationPosition := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_SetOverlayNotificationPosition');
	ISteamGameServerUtils_IsAPICallCompleted := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_IsAPICallCompleted');
	ISteamGameServerUtils_GetAPICallFailureReason := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetAPICallFailureReason');
	ISteamGameServerUtils_GetAPICallResult := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetAPICallResult');
	ISteamGameServerUtils_RunFrame := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_RunFrame');
	ISteamGameServerUtils_GetIPCCallCount := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetIPCCallCount');
	ISteamGameServerUtils_SetWarningMessageHook := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_SetWarningMessageHook');
	ISteamGameServerUtils_IsOverlayEnabled := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_IsOverlayEnabled');
	ISteamGameServerUtils_BOverlayNeedsPresent := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_BOverlayNeedsPresent');
	ISteamGameServerUtils_ShowGamepadTextInput := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_ShowGamepadTextInput');
	ISteamGameServerUtils_GetEnteredGamepadTextLength := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetEnteredGamepadTextLength');
	ISteamGameServerUtils_GetEnteredGamepadTextInput := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetEnteredGamepadTextInput');
	ISteamGameServerUtils_GetSteamUILanguage := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_GetSteamUILanguage');
	ISteamGameServerUtils_IsSteamRunningInVR := GetProcAddress(SteamHandle, 'ISteamGameServerUtils_IsSteamRunningInVR');
	ISteamHTMLSurface_Init := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_Init');
	ISteamHTMLSurface_Shutdown := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_Shutdown');
	ISteamHTMLSurface_CreateBrowser := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_CreateBrowser');
	ISteamHTMLSurface_RemoveBrowser := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_RemoveBrowser');
	ISteamHTMLSurface_LoadURL := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_LoadURL');
	ISteamHTMLSurface_SetSize := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_SetSize');
	ISteamHTMLSurface_StopLoad := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_StopLoad');
	ISteamHTMLSurface_Reload := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_Reload');
	ISteamHTMLSurface_GoBack := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_GoBack');
	ISteamHTMLSurface_GoForward := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_GoForward');
	ISteamHTMLSurface_AddHeader := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_AddHeader');
	ISteamHTMLSurface_ExecuteJavascript := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_ExecuteJavascript');
	ISteamHTMLSurface_MouseUp := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_MouseUp');
	ISteamHTMLSurface_MouseDown := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_MouseDown');
	ISteamHTMLSurface_MouseDoubleClick := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_MouseDoubleClick');
	ISteamHTMLSurface_MouseMove := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_MouseMove');
	ISteamHTMLSurface_MouseWheel := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_MouseWheel');
	ISteamHTMLSurface_KeyDown := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_KeyDown');
	ISteamHTMLSurface_KeyUp := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_KeyUp');
	ISteamHTMLSurface_KeyChar := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_KeyChar');
	ISteamHTMLSurface_SetHorizontalScroll := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_SetHorizontalScroll');
	ISteamHTMLSurface_SetVerticalScroll := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_SetVerticalScroll');
	ISteamHTMLSurface_SetKeyFocus := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_SetKeyFocus');
	ISteamHTMLSurface_ViewSource := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_ViewSource');
	ISteamHTMLSurface_CopyToClipboard := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_CopyToClipboard');
	ISteamHTMLSurface_PasteFromClipboard := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_PasteFromClipboard');
	ISteamHTMLSurface_Find := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_Find');
	ISteamHTMLSurface_StopFind := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_StopFind');
	ISteamHTMLSurface_GetLinkAtPosition := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_GetLinkAtPosition');
	ISteamHTMLSurface_SetCookie := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_SetCookie');
	ISteamHTMLSurface_SetPageScaleFactor := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_SetPageScaleFactor');
	ISteamHTMLSurface_AllowStartRequest := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_AllowStartRequest');
	ISteamHTMLSurface_JSDialogResponse := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_JSDialogResponse');
	ISteamHTMLSurface_FileLoadDialogResponse := GetProcAddress(SteamHandle, 'ISteamHTMLSurface_FileLoadDialogResponse');
	ISteamHTTP_CreateHTTPRequest := GetProcAddress(SteamHandle, 'ISteamHTTP_CreateHTTPRequest');
	ISteamHTTP_SetHTTPRequestContextValue := GetProcAddress(SteamHandle, 'ISteamHTTP_SetHTTPRequestContextValue');
	ISteamHTTP_SetHTTPRequestNetworkActivityTimeout := GetProcAddress(SteamHandle, 'ISteamHTTP_SetHTTPRequestNetworkActivityTimeout');
	ISteamHTTP_SetHTTPRequestHeaderValue := GetProcAddress(SteamHandle, 'ISteamHTTP_SetHTTPRequestHeaderValue');
	ISteamHTTP_SetHTTPRequestGetOrPostParameter := GetProcAddress(SteamHandle, 'ISteamHTTP_SetHTTPRequestGetOrPostParameter');
	ISteamHTTP_SendHTTPRequest := GetProcAddress(SteamHandle, 'ISteamHTTP_SendHTTPRequest');
	ISteamHTTP_SendHTTPRequestAndStreamResponse := GetProcAddress(SteamHandle, 'ISteamHTTP_SendHTTPRequestAndStreamResponse');
	ISteamHTTP_DeferHTTPRequest := GetProcAddress(SteamHandle, 'ISteamHTTP_DeferHTTPRequest');
	ISteamHTTP_PrioritizeHTTPRequest := GetProcAddress(SteamHandle, 'ISteamHTTP_PrioritizeHTTPRequest');
	ISteamHTTP_GetHTTPResponseHeaderSize := GetProcAddress(SteamHandle, 'ISteamHTTP_GetHTTPResponseHeaderSize');
	ISteamHTTP_GetHTTPResponseHeaderValue := GetProcAddress(SteamHandle, 'ISteamHTTP_GetHTTPResponseHeaderValue');
	ISteamHTTP_GetHTTPResponseBodySize := GetProcAddress(SteamHandle, 'ISteamHTTP_GetHTTPResponseBodySize');
	ISteamHTTP_GetHTTPResponseBodyData := GetProcAddress(SteamHandle, 'ISteamHTTP_GetHTTPResponseBodyData');
	ISteamHTTP_GetHTTPStreamingResponseBodyData := GetProcAddress(SteamHandle, 'ISteamHTTP_GetHTTPStreamingResponseBodyData');
	ISteamHTTP_ReleaseHTTPRequest := GetProcAddress(SteamHandle, 'ISteamHTTP_ReleaseHTTPRequest');
	ISteamHTTP_GetHTTPDownloadProgressPct := GetProcAddress(SteamHandle, 'ISteamHTTP_GetHTTPDownloadProgressPct');
	ISteamHTTP_SetHTTPRequestRawPostBody := GetProcAddress(SteamHandle, 'ISteamHTTP_SetHTTPRequestRawPostBody');
	ISteamMatchmaking_GetFavoriteGameCount := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetFavoriteGameCount');
	ISteamMatchmaking_GetFavoriteGame := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetFavoriteGame');
	ISteamMatchmaking_AddFavoriteGame := GetProcAddress(SteamHandle, 'ISteamMatchmaking_AddFavoriteGame');
	ISteamMatchmaking_RemoveFavoriteGame := GetProcAddress(SteamHandle, 'ISteamMatchmaking_RemoveFavoriteGame');
	ISteamMatchmaking_RequestLobbyList := GetProcAddress(SteamHandle, 'ISteamMatchmaking_RequestLobbyList');
	ISteamMatchmaking_AddRequestLobbyListStringFilter := GetProcAddress(SteamHandle, 'ISteamMatchmaking_AddRequestLobbyListStringFilter');
	ISteamMatchmaking_AddRequestLobbyListNumericalFilter := GetProcAddress(SteamHandle, 'ISteamMatchmaking_AddRequestLobbyListNumericalFilter');
	ISteamMatchmaking_AddRequestLobbyListNearValueFilter := GetProcAddress(SteamHandle, 'ISteamMatchmaking_AddRequestLobbyListNearValueFilter');
	ISteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable := GetProcAddress(SteamHandle, 'ISteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable');
	ISteamMatchmaking_AddRequestLobbyListDistanceFilter := GetProcAddress(SteamHandle, 'ISteamMatchmaking_AddRequestLobbyListDistanceFilter');
	ISteamMatchmaking_AddRequestLobbyListResultCountFilter := GetProcAddress(SteamHandle, 'ISteamMatchmaking_AddRequestLobbyListResultCountFilter');
	ISteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter := GetProcAddress(SteamHandle, 'ISteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter');
	ISteamMatchmaking_GetLobbyByIndex := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyByIndex');
	ISteamMatchmaking_CreateLobby := GetProcAddress(SteamHandle, 'ISteamMatchmaking_CreateLobby');
	ISteamMatchmaking_JoinLobby := GetProcAddress(SteamHandle, 'ISteamMatchmaking_JoinLobby');
	ISteamMatchmaking_LeaveLobby := GetProcAddress(SteamHandle, 'ISteamMatchmaking_LeaveLobby');
	ISteamMatchmaking_InviteUserToLobby := GetProcAddress(SteamHandle, 'ISteamMatchmaking_InviteUserToLobby');
	ISteamMatchmaking_GetNumLobbyMembers := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetNumLobbyMembers');
	ISteamMatchmaking_GetLobbyMemberByIndex := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyMemberByIndex');
	ISteamMatchmaking_GetLobbyData := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyData');
	ISteamMatchmaking_SetLobbyData := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SetLobbyData');
	ISteamMatchmaking_GetLobbyDataCount := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyDataCount');
	ISteamMatchmaking_GetLobbyDataByIndex := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyDataByIndex');
	ISteamMatchmaking_DeleteLobbyData := GetProcAddress(SteamHandle, 'ISteamMatchmaking_DeleteLobbyData');
	ISteamMatchmaking_GetLobbyMemberData := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyMemberData');
	ISteamMatchmaking_SetLobbyMemberData := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SetLobbyMemberData');
	ISteamMatchmaking_SendLobbyChatMsg := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SendLobbyChatMsg');
	ISteamMatchmaking_GetLobbyChatEntry := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyChatEntry');
	ISteamMatchmaking_RequestLobbyData := GetProcAddress(SteamHandle, 'ISteamMatchmaking_RequestLobbyData');
	ISteamMatchmaking_SetLobbyGameServer := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SetLobbyGameServer');
	ISteamMatchmaking_GetLobbyGameServer := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyGameServer');
	ISteamMatchmaking_SetLobbyMemberLimit := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SetLobbyMemberLimit');
	ISteamMatchmaking_GetLobbyMemberLimit := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyMemberLimit');
	ISteamMatchmaking_SetLobbyType := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SetLobbyType');
	ISteamMatchmaking_SetLobbyJoinable := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SetLobbyJoinable');
	ISteamMatchmaking_GetLobbyOwner := GetProcAddress(SteamHandle, 'ISteamMatchmaking_GetLobbyOwner');
	ISteamMatchmaking_SetLobbyOwner := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SetLobbyOwner');
	ISteamMatchmaking_SetLinkedLobby := GetProcAddress(SteamHandle, 'ISteamMatchmaking_SetLinkedLobby');
	ISteamMatchmakingServers_RequestInternetServerList := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_RequestInternetServerList');
	ISteamMatchmakingServers_RequestLANServerList := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_RequestLANServerList');
	ISteamMatchmakingServers_RequestFriendsServerList := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_RequestFriendsServerList');
	ISteamMatchmakingServers_RequestFavoritesServerList := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_RequestFavoritesServerList');
	ISteamMatchmakingServers_RequestHistoryServerList := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_RequestHistoryServerList');
	ISteamMatchmakingServers_RequestSpectatorServerList := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_RequestSpectatorServerList');
	ISteamMatchmakingServers_ReleaseRequest := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_ReleaseRequest');
	ISteamMatchmakingServers_GetServerDetails := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_GetServerDetails');
	ISteamMatchmakingServers_CancelQuery := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_CancelQuery');
	ISteamMatchmakingServers_RefreshQuery := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_RefreshQuery');
	ISteamMatchmakingServers_IsRefreshing := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_IsRefreshing');
	ISteamMatchmakingServers_GetServerCount := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_GetServerCount');
	ISteamMatchmakingServers_RefreshServer := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_RefreshServer');
	ISteamMatchmakingServers_PingServer := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_PingServer');
	ISteamMatchmakingServers_PlayerDetails := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_PlayerDetails');
	ISteamMatchmakingServers_ServerRules := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_ServerRules');
	ISteamMatchmakingServers_CancelServerQuery := GetProcAddress(SteamHandle, 'ISteamMatchmakingServers_CancelServerQuery');
	ISteamMusic_BIsEnabled := GetProcAddress(SteamHandle, 'ISteamMusic_BIsEnabled');
	ISteamMusic_BIsPlaying := GetProcAddress(SteamHandle, 'ISteamMusic_BIsPlaying');
	ISteamMusic_GetPlaybackStatus := GetProcAddress(SteamHandle, 'ISteamMusic_GetPlaybackStatus');
	ISteamMusic_Play := GetProcAddress(SteamHandle, 'ISteamMusic_Play');
	ISteamMusic_Pause := GetProcAddress(SteamHandle, 'ISteamMusic_Pause');
	ISteamMusic_PlayPrevious := GetProcAddress(SteamHandle, 'ISteamMusic_PlayPrevious');
	ISteamMusic_PlayNext := GetProcAddress(SteamHandle, 'ISteamMusic_PlayNext');
	ISteamMusic_SetVolume := GetProcAddress(SteamHandle, 'ISteamMusic_SetVolume');
	ISteamMusic_GetVolume := GetProcAddress(SteamHandle, 'ISteamMusic_GetVolume');
	ISteamMusicRemote_RegisterSteamMusicRemote := GetProcAddress(SteamHandle, 'ISteamMusicRemote_RegisterSteamMusicRemote');
	ISteamMusicRemote_DeregisterSteamMusicRemote := GetProcAddress(SteamHandle, 'ISteamMusicRemote_DeregisterSteamMusicRemote');
	ISteamMusicRemote_BIsCurrentMusicRemote := GetProcAddress(SteamHandle, 'ISteamMusicRemote_BIsCurrentMusicRemote');
	ISteamMusicRemote_BActivationSuccess := GetProcAddress(SteamHandle, 'ISteamMusicRemote_BActivationSuccess');
	ISteamMusicRemote_SetDisplayName := GetProcAddress(SteamHandle, 'ISteamMusicRemote_SetDisplayName');
	ISteamMusicRemote_SetPNGIcon_64x64 := GetProcAddress(SteamHandle, 'ISteamMusicRemote_SetPNGIcon_64x64');
	ISteamMusicRemote_EnablePlayPrevious := GetProcAddress(SteamHandle, 'ISteamMusicRemote_EnablePlayPrevious');
	ISteamMusicRemote_EnablePlayNext := GetProcAddress(SteamHandle, 'ISteamMusicRemote_EnablePlayNext');
	ISteamMusicRemote_EnableShuffled := GetProcAddress(SteamHandle, 'ISteamMusicRemote_EnableShuffled');
	ISteamMusicRemote_EnableLooped := GetProcAddress(SteamHandle, 'ISteamMusicRemote_EnableLooped');
	ISteamMusicRemote_EnableQueue := GetProcAddress(SteamHandle, 'ISteamMusicRemote_EnableQueue');
	ISteamMusicRemote_EnablePlaylists := GetProcAddress(SteamHandle, 'ISteamMusicRemote_EnablePlaylists');
	ISteamMusicRemote_UpdatePlaybackStatus := GetProcAddress(SteamHandle, 'ISteamMusicRemote_UpdatePlaybackStatus');
	ISteamMusicRemote_UpdateShuffled := GetProcAddress(SteamHandle, 'ISteamMusicRemote_UpdateShuffled');
	ISteamMusicRemote_UpdateLooped := GetProcAddress(SteamHandle, 'ISteamMusicRemote_UpdateLooped');
	ISteamMusicRemote_UpdateVolume := GetProcAddress(SteamHandle, 'ISteamMusicRemote_UpdateVolume');
	ISteamMusicRemote_CurrentEntryWillChange := GetProcAddress(SteamHandle, 'ISteamMusicRemote_CurrentEntryWillChange');
	ISteamMusicRemote_CurrentEntryIsAvailable := GetProcAddress(SteamHandle, 'ISteamMusicRemote_CurrentEntryIsAvailable');
	ISteamMusicRemote_UpdateCurrentEntryText := GetProcAddress(SteamHandle, 'ISteamMusicRemote_UpdateCurrentEntryText');
	ISteamMusicRemote_UpdateCurrentEntryElapsedSeconds := GetProcAddress(SteamHandle, 'ISteamMusicRemote_UpdateCurrentEntryElapsedSeconds');
	ISteamMusicRemote_UpdateCurrentEntryCoverArt := GetProcAddress(SteamHandle, 'ISteamMusicRemote_UpdateCurrentEntryCoverArt');
	ISteamMusicRemote_CurrentEntryDidChange := GetProcAddress(SteamHandle, 'ISteamMusicRemote_CurrentEntryDidChange');
	ISteamMusicRemote_QueueWillChange := GetProcAddress(SteamHandle, 'ISteamMusicRemote_QueueWillChange');
	ISteamMusicRemote_ResetQueueEntries := GetProcAddress(SteamHandle, 'ISteamMusicRemote_ResetQueueEntries');
	ISteamMusicRemote_SetQueueEntry := GetProcAddress(SteamHandle, 'ISteamMusicRemote_SetQueueEntry');
	ISteamMusicRemote_SetCurrentQueueEntry := GetProcAddress(SteamHandle, 'ISteamMusicRemote_SetCurrentQueueEntry');
	ISteamMusicRemote_QueueDidChange := GetProcAddress(SteamHandle, 'ISteamMusicRemote_QueueDidChange');
	ISteamMusicRemote_PlaylistWillChange := GetProcAddress(SteamHandle, 'ISteamMusicRemote_PlaylistWillChange');
	ISteamMusicRemote_ResetPlaylistEntries := GetProcAddress(SteamHandle, 'ISteamMusicRemote_ResetPlaylistEntries');
	ISteamMusicRemote_SetPlaylistEntry := GetProcAddress(SteamHandle, 'ISteamMusicRemote_SetPlaylistEntry');
	ISteamMusicRemote_SetCurrentPlaylistEntry := GetProcAddress(SteamHandle, 'ISteamMusicRemote_SetCurrentPlaylistEntry');
	ISteamMusicRemote_PlaylistDidChange := GetProcAddress(SteamHandle, 'ISteamMusicRemote_PlaylistDidChange');
	ISteamNetworking_SendP2PPacket := GetProcAddress(SteamHandle, 'ISteamNetworking_SendP2PPacket');
	ISteamNetworking_IsP2PPacketAvailable := GetProcAddress(SteamHandle, 'ISteamNetworking_IsP2PPacketAvailable');
	ISteamNetworking_ReadP2PPacket := GetProcAddress(SteamHandle, 'ISteamNetworking_ReadP2PPacket');
	ISteamNetworking_AcceptP2PSessionWithUser := GetProcAddress(SteamHandle, 'ISteamNetworking_AcceptP2PSessionWithUser');
	ISteamNetworking_CloseP2PSessionWithUser := GetProcAddress(SteamHandle, 'ISteamNetworking_CloseP2PSessionWithUser');
	ISteamNetworking_CloseP2PChannelWithUser := GetProcAddress(SteamHandle, 'ISteamNetworking_CloseP2PChannelWithUser');
	ISteamNetworking_GetP2PSessionState := GetProcAddress(SteamHandle, 'ISteamNetworking_GetP2PSessionState');
	ISteamNetworking_AllowP2PPacketRelay := GetProcAddress(SteamHandle, 'ISteamNetworking_AllowP2PPacketRelay');
	ISteamNetworking_CreateListenSocket := GetProcAddress(SteamHandle, 'ISteamNetworking_CreateListenSocket');
	ISteamNetworking_CreateP2PConnectionSocket := GetProcAddress(SteamHandle, 'ISteamNetworking_CreateP2PConnectionSocket');
	ISteamNetworking_CreateConnectionSocket := GetProcAddress(SteamHandle, 'ISteamNetworking_CreateConnectionSocket');
	ISteamNetworking_DestroySocket := GetProcAddress(SteamHandle, 'ISteamNetworking_DestroySocket');
	ISteamNetworking_DestroyListenSocket := GetProcAddress(SteamHandle, 'ISteamNetworking_DestroyListenSocket');
	ISteamNetworking_SendDataOnSocket := GetProcAddress(SteamHandle, 'ISteamNetworking_SendDataOnSocket');
	ISteamNetworking_IsDataAvailableOnSocket := GetProcAddress(SteamHandle, 'ISteamNetworking_IsDataAvailableOnSocket');
	ISteamNetworking_RetrieveDataFromSocket := GetProcAddress(SteamHandle, 'ISteamNetworking_RetrieveDataFromSocket');
	ISteamNetworking_IsDataAvailable := GetProcAddress(SteamHandle, 'ISteamNetworking_IsDataAvailable');
	ISteamNetworking_RetrieveData := GetProcAddress(SteamHandle, 'ISteamNetworking_RetrieveData');
	ISteamNetworking_GetSocketInfo := GetProcAddress(SteamHandle, 'ISteamNetworking_GetSocketInfo');
	ISteamNetworking_GetListenSocketInfo := GetProcAddress(SteamHandle, 'ISteamNetworking_GetListenSocketInfo');
	ISteamNetworking_GetSocketConnectionType := GetProcAddress(SteamHandle, 'ISteamNetworking_GetSocketConnectionType');
	ISteamNetworking_GetMaxPacketSize := GetProcAddress(SteamHandle, 'ISteamNetworking_GetMaxPacketSize');
	ISteamRemoteStorage_FileWrite := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileWrite');
	ISteamRemoteStorage_FileRead := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileRead');
	ISteamRemoteStorage_FileForget := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileForget');
	ISteamRemoteStorage_FileDelete := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileDelete');
	ISteamRemoteStorage_FileShare := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileShare');
	ISteamRemoteStorage_SetSyncPlatforms := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_SetSyncPlatforms');
	ISteamRemoteStorage_FileWriteStreamOpen := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileWriteStreamOpen');
	ISteamRemoteStorage_FileWriteStreamWriteChunk := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileWriteStreamWriteChunk');
	ISteamRemoteStorage_FileWriteStreamClose := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileWriteStreamClose');
	ISteamRemoteStorage_FileWriteStreamCancel := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileWriteStreamCancel');
	ISteamRemoteStorage_FileExists := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FileExists');
	ISteamRemoteStorage_FilePersisted := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_FilePersisted');
	ISteamRemoteStorage_GetFileSize := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetFileSize');
	ISteamRemoteStorage_GetFileTimestamp := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetFileTimestamp');
	ISteamRemoteStorage_GetSyncPlatforms := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetSyncPlatforms');
	ISteamRemoteStorage_GetFileCount := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetFileCount');
	ISteamRemoteStorage_GetFileNameAndSize := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetFileNameAndSize');
	ISteamRemoteStorage_GetQuota := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetQuota');
	ISteamRemoteStorage_IsCloudEnabledForAccount := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_IsCloudEnabledForAccount');
	ISteamRemoteStorage_IsCloudEnabledForApp := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_IsCloudEnabledForApp');
	ISteamRemoteStorage_SetCloudEnabledForApp := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_SetCloudEnabledForApp');
	ISteamRemoteStorage_UGCDownload := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UGCDownload');
	ISteamRemoteStorage_GetUGCDownloadProgress := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetUGCDownloadProgress');
	ISteamRemoteStorage_GetUGCDetails := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetUGCDetails');
	ISteamRemoteStorage_UGCRead := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UGCRead');
	ISteamRemoteStorage_GetCachedUGCCount := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetCachedUGCCount');
	ISteamRemoteStorage_GetCachedUGCHandle := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetCachedUGCHandle');
	ISteamRemoteStorage_PublishWorkshopFile := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_PublishWorkshopFile');
	ISteamRemoteStorage_CreatePublishedFileUpdateRequest := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_CreatePublishedFileUpdateRequest');
	ISteamRemoteStorage_UpdatePublishedFileFile := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UpdatePublishedFileFile');
	ISteamRemoteStorage_UpdatePublishedFilePreviewFile := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UpdatePublishedFilePreviewFile');
	ISteamRemoteStorage_UpdatePublishedFileTitle := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UpdatePublishedFileTitle');
	ISteamRemoteStorage_UpdatePublishedFileDescription := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UpdatePublishedFileDescription');
	ISteamRemoteStorage_UpdatePublishedFileVisibility := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UpdatePublishedFileVisibility');
	ISteamRemoteStorage_UpdatePublishedFileTags := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UpdatePublishedFileTags');
	ISteamRemoteStorage_CommitPublishedFileUpdate := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_CommitPublishedFileUpdate');
	ISteamRemoteStorage_GetPublishedFileDetails := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetPublishedFileDetails');
	ISteamRemoteStorage_DeletePublishedFile := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_DeletePublishedFile');
	ISteamRemoteStorage_EnumerateUserPublishedFiles := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_EnumerateUserPublishedFiles');
	ISteamRemoteStorage_SubscribePublishedFile := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_SubscribePublishedFile');
	ISteamRemoteStorage_EnumerateUserSubscribedFiles := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_EnumerateUserSubscribedFiles');
	ISteamRemoteStorage_UnsubscribePublishedFile := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UnsubscribePublishedFile');
	ISteamRemoteStorage_UpdatePublishedFileSetChangeDescription := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UpdatePublishedFileSetChangeDescription');
	ISteamRemoteStorage_GetPublishedItemVoteDetails := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetPublishedItemVoteDetails');
	ISteamRemoteStorage_UpdateUserPublishedItemVote := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UpdateUserPublishedItemVote');
	ISteamRemoteStorage_GetUserPublishedItemVoteDetails := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_GetUserPublishedItemVoteDetails');
	ISteamRemoteStorage_EnumerateUserSharedWorkshopFiles := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_EnumerateUserSharedWorkshopFiles');
	ISteamRemoteStorage_PublishVideo := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_PublishVideo');
	ISteamRemoteStorage_SetUserPublishedFileAction := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_SetUserPublishedFileAction');
	ISteamRemoteStorage_EnumeratePublishedFilesByUserAction := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_EnumeratePublishedFilesByUserAction');
	ISteamRemoteStorage_EnumeratePublishedWorkshopFiles := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_EnumeratePublishedWorkshopFiles');
	ISteamRemoteStorage_UGCDownloadToLocation := GetProcAddress(SteamHandle, 'ISteamRemoteStorage_UGCDownloadToLocation');
	ISteamScreenshots_WriteScreenshot := GetProcAddress(SteamHandle, 'ISteamScreenshots_WriteScreenshot');
	ISteamScreenshots_AddScreenshotToLibrary := GetProcAddress(SteamHandle, 'ISteamScreenshots_AddScreenshotToLibrary');
	ISteamScreenshots_TriggerScreenshot := GetProcAddress(SteamHandle, 'ISteamScreenshots_TriggerScreenshot');
	ISteamScreenshots_HookScreenshots := GetProcAddress(SteamHandle, 'ISteamScreenshots_HookScreenshots');
	ISteamScreenshots_SetLocation := GetProcAddress(SteamHandle, 'ISteamScreenshots_SetLocation');
	ISteamScreenshots_TagUser := GetProcAddress(SteamHandle, 'ISteamScreenshots_TagUser');
	ISteamScreenshots_TagPublishedFile := GetProcAddress(SteamHandle, 'ISteamScreenshots_TagPublishedFile');
	ISteamUGC_CreateQueryUserUGCRequest := GetProcAddress(SteamHandle, 'ISteamUGC_CreateQueryUserUGCRequest');
	ISteamUGC_CreateQueryAllUGCRequest := GetProcAddress(SteamHandle, 'ISteamUGC_CreateQueryAllUGCRequest');
	ISteamUGC_SendQueryUGCRequest := GetProcAddress(SteamHandle, 'ISteamUGC_SendQueryUGCRequest');
	ISteamUGC_GetQueryUGCResult := GetProcAddress(SteamHandle, 'ISteamUGC_GetQueryUGCResult');
	ISteamUGC_ReleaseQueryUGCRequest := GetProcAddress(SteamHandle, 'ISteamUGC_ReleaseQueryUGCRequest');
	ISteamUGC_AddRequiredTag := GetProcAddress(SteamHandle, 'ISteamUGC_AddRequiredTag');
	ISteamUGC_AddExcludedTag := GetProcAddress(SteamHandle, 'ISteamUGC_AddExcludedTag');
	ISteamUGC_SetReturnLongDescription := GetProcAddress(SteamHandle, 'ISteamUGC_SetReturnLongDescription');
	ISteamUGC_SetReturnTotalOnly := GetProcAddress(SteamHandle, 'ISteamUGC_SetReturnTotalOnly');
	ISteamUGC_SetAllowCachedResponse := GetProcAddress(SteamHandle, 'ISteamUGC_SetAllowCachedResponse');
	ISteamUGC_SetCloudFileNameFilter := GetProcAddress(SteamHandle, 'ISteamUGC_SetCloudFileNameFilter');
	ISteamUGC_SetMatchAnyTag := GetProcAddress(SteamHandle, 'ISteamUGC_SetMatchAnyTag');
	ISteamUGC_SetSearchText := GetProcAddress(SteamHandle, 'ISteamUGC_SetSearchText');
	ISteamUGC_SetRankedByTrendDays := GetProcAddress(SteamHandle, 'ISteamUGC_SetRankedByTrendDays');
	ISteamUGC_RequestUGCDetails := GetProcAddress(SteamHandle, 'ISteamUGC_RequestUGCDetails');
	ISteamUGC_CreateItem := GetProcAddress(SteamHandle, 'ISteamUGC_CreateItem');
	ISteamUGC_StartItemUpdate := GetProcAddress(SteamHandle, 'ISteamUGC_StartItemUpdate');
	ISteamUGC_SetItemTitle := GetProcAddress(SteamHandle, 'ISteamUGC_SetItemTitle');
	ISteamUGC_SetItemDescription := GetProcAddress(SteamHandle, 'ISteamUGC_SetItemDescription');
	ISteamUGC_SetItemVisibility := GetProcAddress(SteamHandle, 'ISteamUGC_SetItemVisibility');
	ISteamUGC_SetItemTags := GetProcAddress(SteamHandle, 'ISteamUGC_SetItemTags');
	ISteamUGC_SetItemContent := GetProcAddress(SteamHandle, 'ISteamUGC_SetItemContent');
	ISteamUGC_SetItemPreview := GetProcAddress(SteamHandle, 'ISteamUGC_SetItemPreview');
	ISteamUGC_SubmitItemUpdate := GetProcAddress(SteamHandle, 'ISteamUGC_SubmitItemUpdate');
	ISteamUGC_GetItemUpdateProgress := GetProcAddress(SteamHandle, 'ISteamUGC_GetItemUpdateProgress');
	ISteamUGC_SubscribeItem := GetProcAddress(SteamHandle, 'ISteamUGC_SubscribeItem');
	ISteamUGC_UnsubscribeItem := GetProcAddress(SteamHandle, 'ISteamUGC_UnsubscribeItem');
	ISteamUGC_GetNumSubscribedItems := GetProcAddress(SteamHandle, 'ISteamUGC_GetNumSubscribedItems');
	ISteamUGC_GetSubscribedItems := GetProcAddress(SteamHandle, 'ISteamUGC_GetSubscribedItems');
	ISteamUGC_GetItemInstallInfo := GetProcAddress(SteamHandle, 'ISteamUGC_GetItemInstallInfo');
	ISteamUGC_GetItemUpdateInfo := GetProcAddress(SteamHandle, 'ISteamUGC_GetItemUpdateInfo');
	ISteamUnifiedMessages_SendMethod := GetProcAddress(SteamHandle, 'ISteamUnifiedMessages_SendMethod');
	ISteamUnifiedMessages_GetMethodResponseInfo := GetProcAddress(SteamHandle, 'ISteamUnifiedMessages_GetMethodResponseInfo');
	ISteamUnifiedMessages_GetMethodResponseData := GetProcAddress(SteamHandle, 'ISteamUnifiedMessages_GetMethodResponseData');
	ISteamUnifiedMessages_ReleaseMethod := GetProcAddress(SteamHandle, 'ISteamUnifiedMessages_ReleaseMethod');
	ISteamUnifiedMessages_SendNotification := GetProcAddress(SteamHandle, 'ISteamUnifiedMessages_SendNotification');
	ISteamUser_GetHSteamUser := GetProcAddress(SteamHandle, 'ISteamUser_GetHSteamUser');
	ISteamUser_BLoggedOn := GetProcAddress(SteamHandle, 'ISteamUser_BLoggedOn');
	ISteamUser_GetSteamID := GetProcAddress(SteamHandle, 'ISteamUser_GetSteamID');
	ISteamUser_InitiateGameConnection := GetProcAddress(SteamHandle, 'ISteamUser_InitiateGameConnection');
	ISteamUser_TerminateGameConnection := GetProcAddress(SteamHandle, 'ISteamUser_TerminateGameConnection');
	ISteamUser_TrackAppUsageEvent := GetProcAddress(SteamHandle, 'ISteamUser_TrackAppUsageEvent');
	ISteamUser_GetUserDataFolder := GetProcAddress(SteamHandle, 'ISteamUser_GetUserDataFolder');
	ISteamUser_StartVoiceRecording := GetProcAddress(SteamHandle, 'ISteamUser_StartVoiceRecording');
	ISteamUser_StopVoiceRecording := GetProcAddress(SteamHandle, 'ISteamUser_StopVoiceRecording');
	ISteamUser_GetAvailableVoice := GetProcAddress(SteamHandle, 'ISteamUser_GetAvailableVoice');
	ISteamUser_GetVoice := GetProcAddress(SteamHandle, 'ISteamUser_GetVoice');
	ISteamUser_DecompressVoice := GetProcAddress(SteamHandle, 'ISteamUser_DecompressVoice');
	ISteamUser_GetVoiceOptimalSampleRate := GetProcAddress(SteamHandle, 'ISteamUser_GetVoiceOptimalSampleRate');
	ISteamUser_GetAuthSessionTicket := GetProcAddress(SteamHandle, 'ISteamUser_GetAuthSessionTicket');
	ISteamUser_BeginAuthSession := GetProcAddress(SteamHandle, 'ISteamUser_BeginAuthSession');
	ISteamUser_EndAuthSession := GetProcAddress(SteamHandle, 'ISteamUser_EndAuthSession');
	ISteamUser_CancelAuthTicket := GetProcAddress(SteamHandle, 'ISteamUser_CancelAuthTicket');
	ISteamUser_UserHasLicenseForApp := GetProcAddress(SteamHandle, 'ISteamUser_UserHasLicenseForApp');
	ISteamUser_BIsBehindNAT := GetProcAddress(SteamHandle, 'ISteamUser_BIsBehindNAT');
	ISteamUser_AdvertiseGame := GetProcAddress(SteamHandle, 'ISteamUser_AdvertiseGame');
	ISteamUser_RequestEncryptedAppTicket := GetProcAddress(SteamHandle, 'ISteamUser_RequestEncryptedAppTicket');
	ISteamUser_GetEncryptedAppTicket := GetProcAddress(SteamHandle, 'ISteamUser_GetEncryptedAppTicket');
	ISteamUser_GetGameBadgeLevel := GetProcAddress(SteamHandle, 'ISteamUser_GetGameBadgeLevel');
	ISteamUser_GetPlayerSteamLevel := GetProcAddress(SteamHandle, 'ISteamUser_GetPlayerSteamLevel');
	ISteamUserStats_RequestCurrentStats := GetProcAddress(SteamHandle, 'ISteamUserStats_RequestCurrentStats');
	ISteamUserStats_GetStat := GetProcAddress(SteamHandle, 'ISteamUserStats_GetStat');
	ISteamUserStats_GetStat_ := GetProcAddress(SteamHandle, 'ISteamUserStats_GetStat_');
	ISteamUserStats_SetStat := GetProcAddress(SteamHandle, 'ISteamUserStats_SetStat');
	ISteamUserStats_SetStat_ := GetProcAddress(SteamHandle, 'ISteamUserStats_SetStat_');
	ISteamUserStats_UpdateAvgRateStat := GetProcAddress(SteamHandle, 'ISteamUserStats_UpdateAvgRateStat');
	ISteamUserStats_GetAchievement := GetProcAddress(SteamHandle, 'ISteamUserStats_GetAchievement');
	ISteamUserStats_SetAchievement := GetProcAddress(SteamHandle, 'ISteamUserStats_SetAchievement');
	ISteamUserStats_ClearAchievement := GetProcAddress(SteamHandle, 'ISteamUserStats_ClearAchievement');
	ISteamUserStats_GetAchievementAndUnlockTime := GetProcAddress(SteamHandle, 'ISteamUserStats_GetAchievementAndUnlockTime');
	ISteamUserStats_StoreStats := GetProcAddress(SteamHandle, 'ISteamUserStats_StoreStats');
	ISteamUserStats_GetAchievementIcon := GetProcAddress(SteamHandle, 'ISteamUserStats_GetAchievementIcon');
	ISteamUserStats_GetAchievementDisplayAttribute := GetProcAddress(SteamHandle, 'ISteamUserStats_GetAchievementDisplayAttribute');
	ISteamUserStats_IndicateAchievementProgress := GetProcAddress(SteamHandle, 'ISteamUserStats_IndicateAchievementProgress');
	ISteamUserStats_GetNumAchievements := GetProcAddress(SteamHandle, 'ISteamUserStats_GetNumAchievements');
	ISteamUserStats_GetAchievementName := GetProcAddress(SteamHandle, 'ISteamUserStats_GetAchievementName');
	ISteamUserStats_RequestUserStats := GetProcAddress(SteamHandle, 'ISteamUserStats_RequestUserStats');
	ISteamUserStats_GetUserStat := GetProcAddress(SteamHandle, 'ISteamUserStats_GetUserStat');
	ISteamUserStats_GetUserStat_ := GetProcAddress(SteamHandle, 'ISteamUserStats_GetUserStat_');
	ISteamUserStats_GetUserAchievement := GetProcAddress(SteamHandle, 'ISteamUserStats_GetUserAchievement');
	ISteamUserStats_GetUserAchievementAndUnlockTime := GetProcAddress(SteamHandle, 'ISteamUserStats_GetUserAchievementAndUnlockTime');
	ISteamUserStats_ResetAllStats := GetProcAddress(SteamHandle, 'ISteamUserStats_ResetAllStats');
	ISteamUserStats_FindOrCreateLeaderboard := GetProcAddress(SteamHandle, 'ISteamUserStats_FindOrCreateLeaderboard');
	ISteamUserStats_FindLeaderboard := GetProcAddress(SteamHandle, 'ISteamUserStats_FindLeaderboard');
	ISteamUserStats_GetLeaderboardName := GetProcAddress(SteamHandle, 'ISteamUserStats_GetLeaderboardName');
	ISteamUserStats_GetLeaderboardEntryCount := GetProcAddress(SteamHandle, 'ISteamUserStats_GetLeaderboardEntryCount');
	ISteamUserStats_GetLeaderboardSortMethod := GetProcAddress(SteamHandle, 'ISteamUserStats_GetLeaderboardSortMethod');
	ISteamUserStats_GetLeaderboardDisplayType := GetProcAddress(SteamHandle, 'ISteamUserStats_GetLeaderboardDisplayType');
	ISteamUserStats_DownloadLeaderboardEntries := GetProcAddress(SteamHandle, 'ISteamUserStats_DownloadLeaderboardEntries');
	ISteamUserStats_DownloadLeaderboardEntriesForUsers := GetProcAddress(SteamHandle, 'ISteamUserStats_DownloadLeaderboardEntriesForUsers');
	ISteamUserStats_GetDownloadedLeaderboardEntry := GetProcAddress(SteamHandle, 'ISteamUserStats_GetDownloadedLeaderboardEntry');
	ISteamUserStats_UploadLeaderboardScore := GetProcAddress(SteamHandle, 'ISteamUserStats_UploadLeaderboardScore');
	ISteamUserStats_AttachLeaderboardUGC := GetProcAddress(SteamHandle, 'ISteamUserStats_AttachLeaderboardUGC');
	ISteamUserStats_GetNumberOfCurrentPlayers := GetProcAddress(SteamHandle, 'ISteamUserStats_GetNumberOfCurrentPlayers');
	ISteamUserStats_RequestGlobalAchievementPercentages := GetProcAddress(SteamHandle, 'ISteamUserStats_RequestGlobalAchievementPercentages');
	ISteamUserStats_GetMostAchievedAchievementInfo := GetProcAddress(SteamHandle, 'ISteamUserStats_GetMostAchievedAchievementInfo');
	ISteamUserStats_GetNextMostAchievedAchievementInfo := GetProcAddress(SteamHandle, 'ISteamUserStats_GetNextMostAchievedAchievementInfo');
	ISteamUserStats_GetAchievementAchievedPercent := GetProcAddress(SteamHandle, 'ISteamUserStats_GetAchievementAchievedPercent');
	ISteamUserStats_RequestGlobalStats := GetProcAddress(SteamHandle, 'ISteamUserStats_RequestGlobalStats');
	ISteamUserStats_GetGlobalStat := GetProcAddress(SteamHandle, 'ISteamUserStats_GetGlobalStat');
	ISteamUserStats_GetGlobalStat_ := GetProcAddress(SteamHandle, 'ISteamUserStats_GetGlobalStat_');
	ISteamUserStats_GetGlobalStatHistory := GetProcAddress(SteamHandle, 'ISteamUserStats_GetGlobalStatHistory');
	ISteamUserStats_GetGlobalStatHistory_ := GetProcAddress(SteamHandle, 'ISteamUserStats_GetGlobalStatHistory_');
	ISteamUtils_GetSecondsSinceAppActive := GetProcAddress(SteamHandle, 'ISteamUtils_GetSecondsSinceAppActive');
	ISteamUtils_GetSecondsSinceComputerActive := GetProcAddress(SteamHandle, 'ISteamUtils_GetSecondsSinceComputerActive');
	ISteamUtils_GetConnectedUniverse := GetProcAddress(SteamHandle, 'ISteamUtils_GetConnectedUniverse');
	ISteamUtils_GetServerRealTime := GetProcAddress(SteamHandle, 'ISteamUtils_GetServerRealTime');
	ISteamUtils_GetIPCountry := GetProcAddress(SteamHandle, 'ISteamUtils_GetIPCountry');
	ISteamUtils_GetImageSize := GetProcAddress(SteamHandle, 'ISteamUtils_GetImageSize');
	ISteamUtils_GetImageRGBA := GetProcAddress(SteamHandle, 'ISteamUtils_GetImageRGBA');
	ISteamUtils_GetCSERIPPort := GetProcAddress(SteamHandle, 'ISteamUtils_GetCSERIPPort');
	ISteamUtils_GetCurrentBatteryPower := GetProcAddress(SteamHandle, 'ISteamUtils_GetCurrentBatteryPower');
	ISteamUtils_GetAppID := GetProcAddress(SteamHandle, 'ISteamUtils_GetAppID');
	ISteamUtils_SetOverlayNotificationPosition := GetProcAddress(SteamHandle, 'ISteamUtils_SetOverlayNotificationPosition');
	ISteamUtils_IsAPICallCompleted := GetProcAddress(SteamHandle, 'ISteamUtils_IsAPICallCompleted');
	ISteamUtils_GetAPICallFailureReason := GetProcAddress(SteamHandle, 'ISteamUtils_GetAPICallFailureReason');
	ISteamUtils_GetAPICallResult := GetProcAddress(SteamHandle, 'ISteamUtils_GetAPICallResult');
	ISteamUtils_RunFrame := GetProcAddress(SteamHandle, 'ISteamUtils_RunFrame');
	ISteamUtils_GetIPCCallCount := GetProcAddress(SteamHandle, 'ISteamUtils_GetIPCCallCount');
	ISteamUtils_SetWarningMessageHook := GetProcAddress(SteamHandle, 'ISteamUtils_SetWarningMessageHook');
	ISteamUtils_IsOverlayEnabled := GetProcAddress(SteamHandle, 'ISteamUtils_IsOverlayEnabled');
	ISteamUtils_BOverlayNeedsPresent := GetProcAddress(SteamHandle, 'ISteamUtils_BOverlayNeedsPresent');
	ISteamUtils_ShowGamepadTextInput := GetProcAddress(SteamHandle, 'ISteamUtils_ShowGamepadTextInput');
	ISteamUtils_GetEnteredGamepadTextLength := GetProcAddress(SteamHandle, 'ISteamUtils_GetEnteredGamepadTextLength');
	ISteamUtils_GetEnteredGamepadTextInput := GetProcAddress(SteamHandle, 'ISteamUtils_GetEnteredGamepadTextInput');
	ISteamUtils_GetSteamUILanguage := GetProcAddress(SteamHandle, 'ISteamUtils_GetSteamUILanguage');
	ISteamUtils_IsSteamRunningInVR := GetProcAddress(SteamHandle, 'ISteamUtils_IsSteamRunningInVR');

  Result := True;
End;


End.