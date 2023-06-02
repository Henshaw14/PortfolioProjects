/****** Script for SelectTopNRows command from SSMS  ******/
SELECT TOP (1000) [UniqueID ]
      ,[ParcelID]
      ,[LandUse]
      ,[PropertyAddress]
      ,[SaleDate]
      ,[SalePrice]
      ,[LegalReference]
      ,[SoldAsVacant]
      ,[OwnerName]
      ,[OwnerAddress]
      ,[Acreage]
      ,[TaxDistrict]
      ,[LandValue]
      ,[BuildingValue]
      ,[TotalValue]
      ,[YearBuilt]
      ,[Bedrooms]
      ,[FullBath]
      ,[HalfBath]
  FROM [PortfolioProject].[dbo].[Nashville Housing Data]



/*
Cleaning Data in SQL Queries
*/


Select *
From PortfolioProject.dbo.NashvilleHousingData

--------------------------------------------------------------------------------------------------------------------------

-- Standardize Date Format

Select saleDateConverted, CONVERT(Date,SaleDate)
From PortfolioProject.dbo.NashvilleHousingData


Update NashvilleHousingData
SET SaleDate = CONVERT(Date,SaleDate)

-- If it doesn't Update properly

ALTER TABLE NashvilleHousingData
Add SaleDateConverted Date;

Update NashvilleHousingData
SET SaleDateConverted = CONVERT(Date,SaleDate)


 --------------------------------------------------------------------------------------------------------------------------

-- Populate Property Address data

Select *
From PortfolioProject.dbo.NashvilleHousingData
--Where PropertyAddress is null
order by ParcelID



  Update a
SET PropertyAddress = ISNULL(a.PropertyAddress,b.PropertyAddress)
From PortfolioProject.dbo.NashvilleHousingData a
JOIN PortfolioProject.dbo.NashvilleHousingData b
	on a.ParcelID = b.ParcelID
	AND a.[UniqueID ] <> b.[UniqueID ]
Where a.PropertyAddress is null




--------------------------------------------------------------------------------------------------------------------------

-- Breaking out Address into Individual Columns (Address, City, State)


Select PropertyAddress
From PortfolioProject.dbo.NashvilleHousingData
--Where PropertyAddress is null
--order by ParcelID

SELECT
SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1 ) as Address
, SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) + 1 , LEN(PropertyAddress)) as Address

From PortfolioProject.dbo.NashvilleHousingData


ALTER TABLE NashvilleHousingData
Add PropertySplitAddress Nvarchar(255);

Update NashvilleHousingData
SET PropertySplitAddress = SUBSTRING(PropertyAddress, 1, CHARINDEX(',', PropertyAddress) -1 )


ALTER TABLE NashvilleHousingData
Add PropertySplitCity Nvarchar(255);

Update NashvilleHousingData
SET PropertySplitCity = SUBSTRING(PropertyAddress, CHARINDEX(',', PropertyAddress) + 1 , LEN(PropertyAddress))


Select *
From PortfolioProject.dbo.NashvilleHousingData





Select OwnerAddress
From PortfolioProject.dbo.NashvilleHousingData


Select
PARSENAME(REPLACE(OwnerAddress, ',', '.') , 3)
,PARSENAME(REPLACE(OwnerAddress, ',', '.') , 2)
,PARSENAME(REPLACE(OwnerAddress, ',', '.') , 1)
From PortfolioProject.dbo.NashvilleHousingData



ALTER TABLE NashvilleHousingData
Add OwnerSplitAddress Nvarchar(255);

Update NashvilleHousingData
SET OwnerSplitAddress = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 3)


ALTER TABLE NashvilleHousingData
Add OwnerSplitCity Nvarchar(255);

Update NashvilleHousingData
SET OwnerSplitCity = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 2)


ALTER TABLE NashvilleHousingData
Add OwnerSplitState Nvarchar(255);

Update NashvilleHousingData
SET OwnerSplitState = PARSENAME(REPLACE(OwnerAddress, ',', '.') , 1)


Select *
From PortfolioProject.dbo.NashvilleHousingData

--------------------------------------------------------------------------------------------------------------------------


-- Change Y and N to Yes and No in "Sold as Vacant" field


Select Distinct(SoldAsVacant), Count(SoldAsVacant)
From PortfolioProject.dbo.NashvilleHousingData
Group by SoldAsVacant
order by 2



Select SoldAsVacant
, CASE When SoldAsVacant = 'Y' THEN 'Yes'
	   When SoldAsVacant = 'N' THEN 'No'
	   ELSE SoldAsVacant
	   END
From PortfolioProject.dbo.NashvilleHousingData


Update NashvilleHousingData
SET SoldAsVacant = CASE When SoldAsVacant = 'Y' THEN 'Yes'
	   When SoldAsVacant = 'N' THEN 'No'
	   ELSE SoldAsVacant
	   END


-----------------------------------------------------------------------------------------------------------------------------------------------------------

-- Remove Duplicates

WITH RowNumCTE AS(
Select *,
	ROW_NUMBER() OVER (
	PARTITION BY ParcelID,
				 PropertyAddress,
				 SalePrice,
				 SaleDate,
				 LegalReference
				 ORDER BY
					UniqueID
					) row_num

From dbo.NashvilleHousingData
--order by ParcelID
)
select *
From RowNumCTE
Where row_num > 1
Order by PropertyAddress



Select *
From dbo.NashvilleHousingData


---------------------------------------------------------------------------------------------------------

-- Delete Unused Columns



Select *
From PortfolioProject.dbo.NashvilleHousingData


ALTER TABLE PortfolioProject.dbo.NashvilleHousingData
DROP COLUMN OwnerAddress, TaxDistrict, PropertyAddress, SaleDate



-- Data Exploration

SELECT
    UniqueID,
    COUNT(*) AS TotalSales,
    AVG(SalePrice) AS AvgSalePrice,
    MAX(SalePrice) AS MaxSalePrice,
    MIN(SalePrice) AS MinSalePrice,
    SUM(CASE WHEN SoldAsVacant = 'Yes' THEN 1 ELSE 0 END) AS VacantSales,
    AVG(Acreage) AS AvgAcreage,
    SUM(LandValue) AS TotalLandValue,
    SUM(BuildingValue) AS TotalBuildingValue,
    AVG(YearBuilt) AS AvgYearBuilt
FROM
    PortfolioProject.dbo.NashvilleHousingData
GROUP BY
    UniqueID;