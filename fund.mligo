(* A decentralized fundraising platform for decentralized projects *)

// TODO : A governance token for this platform and voting rights/functions
// TODO : Upgradeable project for the decentralised governance 


(* ============================================================================
 * Storage
 * ============================================================================ *)

type storage = 
// keeps track of projects:
// * (project, contributors, votes, state_of_project)
//   * project = (metadata for the website)

(* ============================================================================
 * Entrypoints
 * ============================================================================ *)

type start_project = 
type fund_project = 
type vote = 
| Next_disbursal
| Emergency_fund
type fund_project = 
type vote = 
type give_up = 
type complete_project = (int ticket) * address // the gov token ticket and the gov token address



type entrypoint = 
| Start_project of start_project 
| Update_project of update_project
| Fund_project of fund_project
| Vote of vote 
| Give_up of give_up 
| Complete_project of complete_project


(* ============================================================================
 * Entrypoint Functions 
 * ============================================================================ *)

(*  Start Project  *)





(*  Update Project  *)

// * They just add a new entry of project metadata that will display on the website 
// * Website will still display older versions 
// * This is the way that projects update their users and convince them to not oppose more disbursals
// * 




(*  Fund Project  *)

// 1tez = 1funded token


(*  Vote  *)

// 
// Votes are only negative -- (maybe on the website keep track of positive just for UI but 
// only negative votes can oppose projects)

(*  Give Up  *)



(*  Complete Project   *)

// MAIN IDEA:
// * 10% of funds are held for the project to complete 
// * Entrypoint: get a ticket and an FA2 contract who made that ticket; you'll send over the 
//   big_map of people who have the "contribution" token
// * Community has to authorize (or 50% not oppose) this action; if they oppose it then funds 
//   are returned to them.
// * OTHER OPTION: Could also just use our one. Maybe starting a project has the option of 
//   spinning up a new FA2 contract that will be their gov token or to keep the ledger internally 


// TODO : Community "retribution" for rug pulls 


