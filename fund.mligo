(* A decentralized fundraising platform for decentralized projects *)

// TODO : A governance token for this platform and voting rights/entrypoints/functions
// TODO : Upgradeable project for the decentralised governance 
// TODO : Incentive systems against rug pulls 

// Short-term TODOs:
// TODO : How are rounds updated? How to know timeline? How to know which round you're in?
//        This affects: access_funds and vote and complete_project in particular
// TODO : When voting to restrict funds does this lock the project and it can only be terminated?
// TODO : Keep track of treasury -- either each project gets its own contract or keep track internally 
//        from a security standpoint, each having their own might be best 
//        they would be project smart contracts.

(* ============================================================================
 * Storage
 * ============================================================================ *)

type storage = { 
    projects : (nat, project) big_map ; // project_id -> project
    tokens : ((address, nat), nat) big_map ; // (owner, token_id) -> balance
    next_project_id : nat ; 
}

(* ============================================================================
 * Error codes
 * ============================================================================ *)

[@inline] let error_MUST_HAVE_AT_LEAST_FOUR_DISBURSEMENT_ROUNDS = 0n
[@inline] let error_ONLY_PROJECT_OWNER_CAN_UPDATE_PROJECT_DESCRIPTION = 1n
[@inline] let error_NO_PROJECT_FOUND = 2n
[@inline] let error_TXN_AMT_MUST_BE_0TEZ = 3n
[@inline] let error_NOT_AUTHORIZED_TO_VOTE = 4n
[@inline] let error_FUNDS_ARE_FROZEN_BY_PROJECT_CONTRIBUTORS = 5n
[@inline] let error_NOT_ENOUGH_VOTES_FOR_EMERGENCY_FUNDING = 6n
[@inline] let error_NOT_A_CONTRIBUTOR = 7n
[@inline] let error_NOT_AUTHORIZED_TO_TERMINATE_PROJECT = 8n

(* ============================================================================
 * Entrypoints
 * ============================================================================ *)

type project = { 
    description : string list ; 
    funding_deadline : timestamp ; 
    funding_goal : nat ; 
    funding_acquired : nat ;
    contributors : (address, nat) big_map ;
    disbursement_rounds : int ;
    current_round : int * nat * nat ; // (current_round, votes_next_round, votes_emergency)
    termination_status : nat * bool * (address set) ; // (votes to terminate, termination status)
    timeline : (int, timestamp) map ;
    fa2_addr : address ;
}

type project_id = int
type project_description = string 
type termination_action = 
| ChangeTerminationStatus of unit 
| WithdrawFunds of unit 
| VoteTerminate of unit

type new_project = string * timestamp * nat * int // description, deadline, goal, rounds
type update_project_description = project_id * project_description
type fund_project = project_id
type access_funds =
| NextDisbursal of project_id 
| EmergencyFund of project_id
type reject_funds = project_id * address 
type vote = 
| NextDisbursal of project_id // vote negative
| EmergencyFund of project_id // vote affirmative
type terminate_project = project_id * termination_action
type complete_project = ((unit ticket) * address) option // the gov token ticket and the gov token address


// FA2 types 
type transfer = fa2_from * ((fa2_to * fa2_token_id * fa2_amt) list)
type balance_of = ((fa2_owner * fa2_token_id) list) * ((fa2_owner * fa2_token_id * fa2_amt) list contract)
type update_operators = 
    | Add_operator of fa2_owner * fa2_operator * fa2_token_id
    | Remove_operator of fa2_owner * fa2_operator * fa2_token_id
type mint = (fa2_owner * fa2_token_id * fa2_amt) list
type burn = (fa2_owner * fa2_amt) list
type get_metadata = fa2_token_id list



type entrypoint = 
| NewProject of new_project
| UpdateProjectDescription of update_project_description
| FundProject of fund_project
| AccessFunds of access_funds
| RejectFunds of reject_funds
| Vote of vote 
| TerminateProject of terminate_project 
| CompleteProject of complete_project
// FA2 portion
| Transfer of transfer 
| Balance_of of balance_of
| Update_operators of update_operators
| Mint of mint
| Burn of burn
| Get_metadata of get_metadata


type result = (operation list) * storage

(* ============================================================================
 * Entrypoint Functions 
 * ============================================================================ *)

(*****  Start Project  *****)

let new_project (param : start_proejct) (storage: storage) : result = 
    let (description, funding_deadline, funding_goal, disbursement_rounds) = param in 
    
    if (disbusrement_rounds < 4) then
        (failwith error_DISBURSEMENT_ROUNDS_MUST_BE_GEQ_4 : result) else 
    if (Tezos.amount > 0tez) then 
        (failwith error_TXN_AMT_MUST_BE_0TEZ : result) else
    
    let new_project : project = {
        description = [ description ;] ;
        funding_deadline = funding_deadline ;
        funding_goal = funding_goal ; // in mutez
        funding_acquired = 0_000_000n ; // in mutez
        contributors = (Big_map.empty : (address, nat) big_map) ; 
        disbursement_rounds = disbursement_rounds ;
    } in 

    let updated_storage = { storage with 
        projects = 
            Big_map.update (Tezos.source, storage.next_project_id) (Some new_project) storage.projects ;
        next_project_id = next_project_id + 1n ;
    }

    in (([] : operation list), updated_storage)



(*****  Update Project  *****)

(* Projects will give regular updates by adding to the description of their 
   project; this will display on a UI. This is for accountability, to show 
   investors they are still making progress and still deserve funds. *)

let update_project_description (param : update_project_description) (storage: storage) : result = 
    let (id, next_description) = param in 
    let owner = Tezos.source in 
    if (Tezos.source <> owner) then 
        (failwith error_ONLY_PROJECT_OWNER_CAN_UPDATE_PROJECT_DESCRIPTION : result) else 

    let updated_project = 
    match (Big_map.find_opt (owner, id) storage.projects) with 
    | None -> (failwith error_NO_PROJECT_FOUND : result)
    | Some project -> (
        let updated_description = next_description :: project.description in 
        let updated_project = { project with 
            description = updated_description ;
        } in 
    ) in 
    
    let updated_storage = { storage with 
        projects = Big_map.update (owner,id) (Some updated_project) storage.projects ;
    }
    in (([] : operation list), updated_storage)



(*****  Fund Project  *****)

let fund_project (param : fund_project) (storage: storage) : result = 
    let id = param in 
    let proj : project = (
        match Big_map.find_op id storage.projects with 
        | None -> (failwith error_NO_PROJECT_FOUND : result)
        | Some p -> p
    ) in 
    let contribution_amt = Tezos.amount / 1mutez in // amt always in mutez

    let updated_project = {project with 
        funding_acquired = funding_acquired + contribution_amt ;
        contributors = (Big_map.update id (Some (Tezos.source, contribution_amt)) contributors) ;
    } in 

    let updated_projects = Big_map.update id (Some updated_project) storage.projects in 

    // TODO : 
    //   * Mint contribution_amt number's worth of project tokens; 
    //     so each project comes with an FA2 contract
    //   * 1tez = 1 "gov" token

    (([] : operation list), {storage with projects = updated_projects;})


(*****  Access Funds  *****)

let access_funds (param : access_funds) (storage : storage) : result = 
    match param with 
    | NextDisbursal id -> (
        let proj = (
            match (Big_map.find_op id storage.projects) with 
            | None -> (failwith error_NO_PROJECT_FOUND : result)
            | Some p -> p
        ) in 
        // TODO : checks if this is the right round
        // checks if there are too many negative votes 
        let (round, votes_next_round, votes_emergency) = proj.current_round in 
        if votes_next_round > (proj.funding_acquired / 2n) then 
            (failwith error_FUNDS_ARE_FROZEN_BY_PROJECT_CONTRIBUTORS : result) else 
        // TODO : sends the money of the next round (make room in storage)
        // op_disburse = ... 
        (([] : operation list), storage)
    )
    | EmergencyFund id -> (
        let proj = (
            match (Big_map.find_op id storage.projects) with 
            | None -> (failwith error_NO_PROJECT_FOUND : result)
            | Some p -> p
        ) in 
        // TODO : checks if this is the right round
        // checks if there are enough positive votes 
        let (_, _, votes_emergency) = proj.current_round in 
        if votes_emergency < (proj.funding_acquired / 2n) then 
            (failwith error_NOT_ENOUGH_VOTES_FOR_EMERGENCY_FUNDING : result) else 
        // TODO : 
        //  * sends the money of the next round (make room in storage)
        //  * updates however you're keeping track of rounds; moves to the next round
        // op_disburse = ... 
        (([] : operation list), storage)
    )

(*****  Reject Funds (donations)  *****)

let reject_funds (param : reject_funds) (storage : storage) : result = 
        let (id, addr_to_reject) = param in 
        let proj = (
            match (Big_map.find_op id storage.projects) with 
            | None -> (failwith error_NO_PROJECT_FOUND : result)
            | Some p -> p
        ) in 
        match (Big_map.find_op addr_to_reject proj.contributors) with 
        | None -> (failwith error_NOT_A_CONTRIBUTOR : result)
        | Some amt -> (
            let op_return_funds = 
                Tezos.transaction () addr_to_reject amt in 
            ([op_return_funds], storage)
        )


(*****  Vote  *****)

let vote (param : vote) (storage: storage) : result = 
    match param with // (current_round, votes_next_round, votes_emergency)
    | NextDisbursal id -> ( // vote nay
        let proj = (
            match (Big_map.find_op id storage.projects) with 
            | None -> (failwith error_NO_PROJECT_FOUND : result)
            | Some p -> p
        ) in 
        // check voting eligibility
        match (Big_map.find_op Tezos.source proj.contributors) with
        | None -> (failwith error_NOT_AUTHORIZED_TO_VOTE : result)
        | Some amt -> (
            let (round, votes_next_round, votes_emergency) = proj.current_round in 
            let updated_votes_next_round = votes_next_round + amt in 
            let updated_proj = { proj with
                current_round = (round, updated_votes_next_round, votes_emergency);
            } in 
            let updated_projs = Big_map.update id (Some updated_proj) storage.projects in 
            let updated_storage = {storage with
                projects = updated_projs;
            } in 
            (([] : operation list), updated_storage)
        )
    )
    | EmergencyFund id -> ( // vote yea
        let proj = (
            match (Big_map.find_op id storage.projects) with 
            | None -> (failwith error_NO_PROJECT_FOUND : result)
            | Some p -> p
        ) in 
        // check voting eligibility
        match (Big_map.find_op Tezos.source proj.contributors) with
        | None -> (failwith error_NOT_AUTHORIZED_TO_VOTE : result)
        | Some amt -> (
            let (round, votes_next_round, votes_emergency) = proj.current_round in 
            let updated_votes_emergency = votes_emergency + amt in 
            let updated_proj = { proj with
                current_round = (round, votes_next_round, updated_votes_emergency);
            } in 
            let updated_projs = Big_map.update id (Some updated_proj) storage.projects in 
            let updated_storage = {storage with
                projects = updated_projs;
            } in 
            (([] : operation list), updated_storage)
        )
    )
    (([] : operation list), storage)


(*****  Terminate Project  *****)

let terminate_project (param : terminate_project) (storage : storage) : result = 
    let (id, termination_action) = param in 
    let proj = (
        match (Big_map.find_op id storage.projects) with 
        | None -> (failwith error_NO_PROJECT_FOUND : result)
        | Some p -> p
    ) in 
    let (votes_terminate, status, voted) = proj.termination_status in 

    match termination_action with
    | ChangeTerminationStatus () -> (
        // check failing conditions 
        if (Tezos.source <> proj.owner) && (votes_terminate < (proj.funding_acquired / 2))
        then (failwith error_NOT_AUTHORIZED_TO_TERMINATE_PROJECT : result) else
        
        // change termination status to true 
        let updated_proj = {proj with termination_status = (votes_terminate, true, voted);} in 
        let updated_projs = Big_map.update id (Some updated_proj) storage.projects in 
        let updated_storage = {storage with
            projects = updated_projs;
        } in 
        (([] : operation list), updated_storage)
        )
    | WithdrawFunds () -> (
        let contribution_amt = (
            match (Big_map.find_op Tezos.sender proj.contributors) with 
            | None -> (failwith )
            | Some n -> n
        ) in 
        let refund = contribution_amt in // TODO : make proportional to current treasury amt 
        let op_refund = Tezos.transaction () Tezos.source (1mutez * refund) in 
        ([op_refund] , storage)
        )
    | VoteTerminate () -> (
        if (Set.mem Tezos.source votes) then (failwith ) else
        let voting_power : nat = (
            match (Big_map.find_op Tezos.source proj.congributors) with 
            | None -> (failwith )
            | Some n -> n
        ) in
        let new_termination_status = (votes_terminate + voting_power, status, voted) in 
        let updated_proj = {proj with termination_status = new_termination_status} in 
        let updated_projs = Big_map.update id (Some updated_proj) storage.projects in 
        let updated_storage = {storage with
            projects = updated_projs;
        } in 
        (([] : operation list), updated_storage)
        )


(*****  Complete Project   *****)

let complete_project (param : complete_project ) (storage : storage) : result= 
    match param with 
    | Some (new_fa2_ticket, address) -> (
        // TODO : transfer big map to their specified FA2, using the 
        //        ticket to access the storage on a one-time basis 
    )
    | None -> (
        // spins up a new fa2 ...
        // TODO : should contracts get their own FA2?
    )

    // TODO : 
    //  * either add them as the admins on the FA2 contract attached or transfer all the data to a new FA2
    //  * release funds (the 10% kept behind)
    //  * take commission (maybe save this for later) for the treasury
    //  * make sure community hasn't voted against it (if they have only option is terminate)

    (([] : operation list), storage)



(***** ***** ***** *****
  FA2 Functionality   
 ***** ***** ***** *****)

let rec transfer (param , storage : transfer * storage) : result = 
    let (fa2_from, transfers_list) = param in
    if Tezos.source <> fa2_from then (failwith error_FA2_NOT_OWNER : result) else 
    match transfers_list with
    | hd :: tl ->
        let (fa2_to, fa2_token_id, fa2_amt) = hd in
        let big_map_key = (fa2_from, fa2_token_id) in
        let fa2_ledger = storage.fa2_ledger in
        let sender_token_balance =
            (match (Big_map.find_opt big_map_key fa2_ledger) with
            | None -> 0n
            | Some token_balance -> token_balance)
        in
        if (sender_token_balance < fa2_amt) then (failwith error_FA2_INSUFFICIENT_BALANCE : result) else
        let new_fa2_ledger_0 = (Big_map.update (fa2_from, fa2_token_id) (Some (abs (sender_token_balance - fa2_amt))) storage.fa2_ledger) in 
        let recipient_balance = 
            (match (Big_map.find_opt (fa2_to, fa2_token_id) fa2_ledger) with
            | None -> 0n
            | Some recipient_token_balance -> recipient_token_balance)
        in
        let new_fa2_ledger = (Big_map.update (fa2_to, fa2_token_id) (Some (recipient_balance + fa2_amt)) new_fa2_ledger_0) in 
        let new_storage = {storage with fa2_ledger = new_fa2_ledger} in
        let new_param = (fa2_from, tl) in
        transfer (new_param, new_storage)
    | [] -> (([] : operation list), storage)


let balance_of (param : balance_of) (storage : storage) : result = 
    let (request_list, callback) = param in
    let accumulator = ([] : (fa2_owner * fa2_token_id * fa2_amt) list) in
    let ack_list = owner_and_id_to_balance (accumulator, request_list, storage.fa2_ledger) in
    let t = Tezos.transaction ack_list 0mutez callback in
    ([t], storage)


// fa2_owner adds or removes fa2_operator from storage.operators
let update_operators (param : update_operators) (storage : storage) : result = 
    match param with
    | Add_operator (fa2_owner, fa2_operator, fa2_token_id) ->
        (match (Big_map.find_opt fa2_owner storage.operators) with
        | None -> (failwith error_FA2_NOT_OPERATOR : result)
        | Some result_token_id ->
            if result_token_id <> fa2_token_id then (failwith error_FA2_NOT_OPERATOR : result) else
            let new_operators = Big_map.update fa2_operator (Some fa2_token_id) storage.operators in
            let storage = {storage with operators = new_operators} in 
            (([] : operation list), storage)
        )
    | Remove_operator (fa2_owner, fa2_operator, fa2_token_id) ->
        (match (Big_map.find_opt fa2_owner storage.operators) with
        | None -> (failwith error_FA2_NOT_OPERATOR : result)
        | Some result_token_id -> 
            if result_token_id <> fa2_token_id then (failwith error_FA2_NOT_OPERATOR : result) else
            (match (Big_map.find_opt fa2_operator storage.operators) with
            | None -> (([] : operation list), storage) // Nothing happens
            | Some operator_token_id ->
                if operator_token_id <> fa2_token_id 
                then (([] : operation list), storage) 
                else
                    let new_operators = Big_map.update fa2_operator (None : fa2_token_id option) storage.operators in
                    let storage = {storage with operators = new_operators} in 
                    (([] : operation list), storage)
            )
        )


let rec mint_tokens (param, storage : mint * storage) : result =
    let minting_list = param in
    match minting_list with 
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let (fa2_owner, fa2_token_id, fa2_amt) = hd in
        (match (Big_map.find_opt (Tezos.source, fa2_token_id) storage.fa2_ledger) with
        | None -> (failwith error_FA2_NOT_OPERATOR : result)
        | Some sender_fa2_token_idprivelege -> 
            if sender_fa2_token_idprivelege <> fa2_token_id then (failwith error_FA2_NOT_OPERATOR : result) else
            let fa2_ownerbalance = 
                (match (Big_map.find_opt (fa2_owner, fa2_token_id) storage.fa2_ledger) with
                | None -> 0n
                | Some fa2_ownerprev_balance -> fa2_ownerprev_balance)
            in
            let new_fa2_ownerbalance = fa2_ownerbalance + fa2_amt in
            let new_fa2_ledger = Big_map.update (fa2_owner, fa2_token_id) (Some new_fa2_ownerbalance) storage.fa2_ledger in
            let storage = {storage with fa2_ledger = new_fa2_ledger} in 
            mint_tokens (tl, storage))

let burn (_param : burn) (storage : storage) : result = (([] : operation list), storage) // TODO : Permissions TBD 

let get_metadata (_param : get_metadata) (storage : storage) : result = (([] : operation list), storage) // TODO : Metadata details TBD







(* ============================================================================
 * Main Function 
 * ============================================================================ *)


let main (entrypoint, storage : entrypoint * storage) : result = 
    match entrypoint with 
    | NewProject param ->
        new_project param storage
    | UpdateProjectDescription param -> 
        update_project_description param storage
    | FundProject param -> 
        fund_project param storage
    | AccessFunds param -> 
        access_funds param storage
    | Vote param -> 
        vote param storage
    | TerminateProject param -> 
        terminate_project param storage
    | CompleteProject param -> 
        complete_project param storage
    // FA2 functionality
    | Transfer param ->
        transfer (param, storage)
    | Balance_of param -> 
        balance_of param storage
    | Update_operators param ->
        update_operators param storage
    | Mint param -> 
        mint_tokens (param, storage)
    | Burn param ->
        burn param storage
    | Get_metadata param ->
        get_metadata param storage