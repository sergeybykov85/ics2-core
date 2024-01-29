import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Nat "mo:base/Nat";
import Time "mo:base/Time";
import List "mo:base/List";

module {

	public type AccessList = {owner : Principal; operators : [Principal]};

	public type NameValue = {
		name : Text;
		value : Text;
	};
	
	// state of the bucket or repository itself
	public type BucketInfo = {
		id : Principal;
		name : Text;
		cycles : Int;
		memory_mb : Int;
		heap_mb : Int;
		chunks : Nat;
		files : Nat;
		directories : Nat;
		url : Text;
	};

	public type RequestedObject = {
		// path or id
		path : [Text];
		view_mode : ViewMode;
		token : ?Text;
	};

	public type AccessType = {
		// anyone can read
		#Public;
		// read operation is restricted
		#Private;
	};

	public type AccessKeyArgs = {
		id : Text;
		name : Text;
		entropy : Text;
		valid_to : ?Time.Time;
	};

	public type AccessKey = {
		id : Text;
		// logical name
		name : Text;
		// never shared
		token : ?Text;
		created : Time.Time;
		valid_to : ?Time.Time;
	};

	public type AccessToken = {
		token : Text;
		created: Time.Time;
		valid_to : ?Time.Time;
	};	

	public type Network = {
        #IC;
        #Local: Text; // host details like localhost:4943
    };

	public type ViewMode = {
		#Index;     // index, names could be used as a part of browser url
		#Open;      // references to the resource by its hash
		#Download;  // reference to the resource by its hash, download in browser
	};

	public type ResourceType = {
		#File;
		#Directory;
	};

	public type ChunkBinding = {
		var chunks : List.List<Text>;
		created : Time.Time;
	};

	public type ResourceChunk = {
		content : Blob;
		created : Time.Time;
		id : Text;
		// opportunity to link chunks by a logical name
		binding_key : ?Text;
	};

	// Type object to create a new resource
	public type ResourceArgs = {
		content_type : ?Text;
		name : Text;
		// input argument, directory name
		parent_path : ?Text;
		// direcotry id. It has a precedence over the parent_path, but this field is not supported in all methods
		parent_id : ?Text;
		ttl : ?Nat;
		readonly : ?Nat;
	};

	public type ResourceAction = {
		#Copy;
		#Delete;
		#Rename;
		#TTL;
		#Replace;
		#ReadOnly;
		#HttpHeaders;
	};
	// Type contains possible required data to make some action with an existing resource
	public type ActionResourceArgs = {
		id : Text;
		action : ResourceAction;
		payload : ?Blob;
		name : ?Text;
		parent_path : ?Text;
		ttl : ?Nat;
		readonly : ?Nat;
		http_headers: ?[NameValue];
	};	

	public type Resource = {
		resource_type : ResourceType;
		var http_headers : [(Text, Text)];
		var ttl : ?Nat;
		// time if no modification allowed : remove or replace (if supported)
		var readonly : ?Nat;
		// resource could be replaced (to remain the same URL)
		var content_size : Nat;
		created : Int;
		var updated : ?Int;
		var name : Text;
		// folder reference (hash, not the name)
		var parent : ?Text;
		// references to other resources in case of "folder type"
		var leafs : List.List<Text>;
		// data identifier 
		did : ?Text;		
	};

	public type ResourceView = {
		id : Text;
		resource_type : ResourceType;
		http_headers : [(Text, Text)];
		content_size : Nat;
		ttl : ?Nat;
		created : Int;
		name : Text;
		url : Text;
	};

	public type IdUrl = {
		id : Text;
		url : Text;
		// usually it is a bucket
		partition : Text;
	};

	public type DirectoryView = {
		id : Text;
		total_files : Nat;
		total_size : Nat;
		created : Int;
		url : Text;
	};		

	public type BucketArgs = {
		name : Text;
		network : Network;
		operators : [Principal];
		// propagated from repo
		access_type : AccessType;
		access_token : ?[AccessToken];
	};

	public type CommitArgs = {
		chunks : [Text];
		binding_key: ?Text;
	};

	public type WitdrawArgs = {
		to : Principal;
		// cycles to leave before making the withdraw request
		remainder_cycles :?Nat;
	};

	public type Errors = {
		// Tier is not registered
        #TierNotFound;
		// Tier restriction
        #TierRestriction;		
		// no resource or no chunk
		#NotFound;
		// record already registered
		#DuplicateRecord;
		// action not allowed by the logic or constraints
        #OperationNotAllowed;
        // not registered
        #NotRegistered;
		// when input argument contains wrong value
		#InvalidRequest;
        // exceeded allowed items
        #ExceededAllowedLimit;	
		// not authorized to manage certain object
		#AccessDenied;	
    };

    public type ICSettingsArgs = {
        controllers : ?[Principal];
    };	

    public type ICManagementActor = actor {
        stop_canister : shared { canister_id : Principal } -> async ();
		delete_canister : shared { canister_id : Principal } -> async ();
        update_settings : shared {
            canister_id : Principal;
            settings : ICSettingsArgs;
        } -> async ();
    };

	public type Wallet = actor {
    	wallet_receive : () -> async ();
		withdraw_cycles : shared {to : Principal; remainder_cycles : ?Nat} -> async ();
    };

};
