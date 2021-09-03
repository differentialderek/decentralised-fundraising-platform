# Fund
A Decentralised Fundraising Platform for Decentralised Projects: Building projects owned and funded by the community.

Built on Tezos

## Key Features

This platform is a decentralised way of raising funds for decentralised projects. Investors can choose projects to fund. In exchange for their investment, they receive "investment" tokens, which later mature into governance tokens for the project they funded. Projects initiated will outline a timetable for disbursement of funds, which must be at least four rounds for security purposes; at any point, the stakeholders can vote to terminate the project and get a refund of the funds that remain.

The key feature here is early and active community involvement in a decentralised project. Projects interact with their community in every step of the process. The project has the responsibility of keeping their stakeholders informed on the progress and development of the project. If they ghost their stakeholders, stakeholders have mechanisms in place where they can get a refund on their investment.

On the other side, projects can request early disbursement of funds and the community can vote to approve.

The final disbursement of funds requires the address of the new governance token address and a ticket that allows the `fund.mligo` contract to update the ticket balances one a one-off basis. This mechanism forces a project to airdrop governance tokens to its users in order to receive the final stage of funding.

This funding model will help projects keep their users as first priority, rather than large VC or angel investors that might have other agendas in mind. It also encourages decentralised development. It aligns incentives, giving investors an active role in the early stages of development. This platform is geared toward investors being the future users, like a crowdfunding platform, rather than institutional or special-interest investors.

## Starting A Project 

To start a new project on Fund, an address must provide a project description, a deadline for funding, a funding goal, and a proposed schedule for disbursement of funds. If they raise funds successfully by their deadline, then the address will be able to access funds in tranches, according to the timeline they proposed.

## Accountability 

There are several accounability mechanisms that tie the company's interest to investors, who will ideally be future users:
* Investors can vote during any round to withold funding if the team isn't keeping them up to date with development or is unconvincing in the progress they've made.
* Investors can vote to terminate the project at any time and withdraw (what is left of) their invested funds.
* Rounds of funding must be such that at least two funding rounds must go by before the company receives >50% of funding. This prevents them from buying up the "investment" tokens and blocking any attempts to terminate or restrict funding by the investors.
* In order to access the last round of funding, the project owner must airdrop governance tokens to investors.

## Completing a Project
Every step of the project is governed by investors, so it is the responsibility of the project owner to keep the investors satisfied with their progress in order to keep having access to funds. In order to complete the project, a project must airdrop governance tokens to investors. This aligns incentives to create decentralised projects from decentralised funding: projects funded and owned by the community.