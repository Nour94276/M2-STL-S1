// SPDX-License-Identifier: MIT
pragma solidity ^0.8.20;
import "@openzeppelin/contracts/token/ERC721/ERC721.sol";
import "@openzeppelin/contracts/token/ERC721/extensions/ERC721URIStorage.sol";
import "@openzeppelin/contracts/access/Ownable.sol";

contract Collection is ERC721URIStorage  {
    string public _collectionName;
    uint256 public _cardCount;
    uint256 public _nextTokenId;
    mapping(uint256 => string) private cardMetadata; // Mapping pour stocker les métadonnées des NFT
    string[] public _tokenList;
    constructor(string memory _name, string memory _symbol) ERC721(_name, _symbol) {
        _nextTokenId = 0;
    }
    
    function getAllNFTsWithDetails() external view returns (uint256[] memory, string[] memory, address[] memory) {
    uint256[] memory tokenIds = new uint256[](_nextTokenId);
    string[] memory uris = new string[](_nextTokenId);
    address[] memory owners = new address[](_nextTokenId);

    for (uint256 i = 0; i < _nextTokenId; i++) {
        tokenIds[i] = i;
        uris[i] = tokenURI(i);
        owners[i] = ownerOf(i);
    }

    return (tokenIds, uris, owners);
}

     function mint(string memory tokenURI, address user)
        public
        returns (uint256)
    {
        uint256 newItemId = _nextTokenId;
        _mint(user, newItemId);
        _setTokenURI(newItemId, tokenURI);
        _nextTokenId += 1; 
        _tokenList.push(tokenURI);
        return newItemId;
    }
    function getAllNfts()external view returns(string [] memory)
    {
        return _tokenList ; 
    }
    function setAttributes(string memory name, uint256  cardCount) external{
         _collectionName = name;
         _cardCount = cardCount;
    }

    function getCardMetadata(uint256 tokenId) external view returns (string memory) {
        return cardMetadata[tokenId];
    }

    function getCardsMetadataLength() external view returns (uint256) {
        return _nextTokenId;
    }

    function transferCard(address from , address to, uint256 tokenId) external {
      require(isApprovedOrOwner(from, tokenId), "Vous ne pouvez transferer que vos propres cartes");
      safeTransferFrom(from, to, tokenId);
    }

    function isApprovedOrOwner(address spender, uint256 tokenId) internal view returns (bool) {
        address owner = ownerOf(tokenId);
        return (spender == owner || getApproved(tokenId) == spender || isApprovedForAll(owner, spender));
    }

    event CardAdded(uint256 tokenId, string metadata);

}