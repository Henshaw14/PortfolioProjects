select * from dbo.CovidDeath
where continent  is not null
order by 3, 4

--select * from dbo.CovidVaccination
--order by 3, 4

--Selecting the data that i will be working with
select	location, date, total_cases, new_cases, total_deaths, population
From dbo.CovidDeath
order by 1, 2

--Investigating/Calculating the Total number of Covid cases and the Total deaths
-- Showing the likelihood of dying if you contract covid in your country 
select	location, date, total_cases, total_deaths, (total_deaths/total_cases)*100 as DeathPercentage 
From dbo.CovidDeath
--where continent  is not null
Where location like '%Nigeria%'
order by 1, 2

--Looking at Total cases vs the population
--Showing the percentage of the population that got Covid 
select	location, date, total_cases, total_deaths, population, (total_cases/population)*100 as PercentagePopulationInfected 
From dbo.CovidDeath
where continent  is not null
--Where location like '%Nigeria%'
order by 1, 2

--Country with highest Covid infection rate
select	location, population, Max(total_cases) as HighestInfectionCount, Max((total_cases/population))*100 as PercentagePopulationInfected 
From dbo.CovidDeath
where continent  is not null
--Where location like '%Nigeria%'
Group by Location , population
order by PercentagePopulationInfected desc 


--Showing the countries with the highest death count per population
select	location, Max(cast(total_deaths as int)) as TotalDeathCount
From dbo.CovidDeath
--Where location like '%Nigeria%'
where continent  is not null
Group by Location
order by TotalDeathCount desc 

--Breaking it down by continent 
select	continent, Max(cast(total_deaths as int)) as TotalDeathCount
From dbo.CovidDeath
--Where location like '%Nigeria%'
where continent  is not null
Group by continent
order by TotalDeathCount desc

--Showing continent with the highest death count per population
select	continent, Max(cast(total_deaths as int)) as TotalDeathCount
From dbo.CovidDeath
--Where location like '%Nigeria%'
where continent  is not null
Group by continent
order by TotalDeathCount desc

--Global statistics
select	date, SUM(new_cases) as total_cases, Sum(cast(new_deaths as int)) as total_deaths, sum(cast(new_deaths as int))/Sum(New_cases)*100 as DeathPercentage 
From dbo.CovidDeath
where continent  is not null
--Where location like '%Nigeria%'
Group by date
order by DeathPercentage asc

--Joning two data
select * 
from dbo.CovidDeath dea
join dbo.CovidVaccination vac
on dea.location = vac.location
and dea.date = vac.date

-- Total population vs Vaccination

--CTE
With PopvsVac (Continent, Location, Date, Population, New_Vaccinations, RollingPeopleVaccinated)
as
(
SELECT dea.continent, 
       dea.location, 
       dea.date, 
       dea.population, 
       vac.new_vaccinations,
       SUM(CONVERT(BIGINT, vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY dea.location, dea.date) as RollingPeopleVaccinated
	   --,RollingPeopleVaccinated
FROM dbo.CovidDeath dea
JOIN dbo.CovidVaccination vac
    ON dea.location = vac.location
    AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2, 3
)
select *, (RollingPeopleVaccinated/Population)*100
From PopvsVac


--TEMP TABLE
Drop Table if exists #PercentagePopulationVaccinate
create Table #PercentagePopulationVaccinate
(
Continent nvarchar(255)
,Location nvarchar(255)
,Date datetime
,Population numeric
,New_Vaccinations numeric
,RollingPeopleVaccinated numeric
)

Insert into #PercentagePopulationVaccinate
SELECT dea.continent, 
       dea.location, 
       dea.date, 
       dea.population, 
       vac.new_vaccinations,
       SUM(CONVERT(BIGINT, vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY dea.location, dea.date) as RollingPeopleVaccinated
	   --,RollingPeopleVaccinated
FROM dbo.CovidDeath dea
JOIN dbo.CovidVaccination vac
    ON dea.location = vac.location
    AND dea.date = vac.date
--WHERE dea.continent IS NOT NULL
--ORDER BY 2, 3

select *, (RollingPeopleVaccinated/Population)*100
From #PercentagePopulationVaccinate



-- Creating view to store data for visualization

Create View PercentPopulationVaccinated as
SELECT dea.continent, 
       dea.location, 
       dea.date, 
       dea.population, 
       vac.new_vaccinations,
       SUM(CONVERT(BIGINT, vac.new_vaccinations)) OVER (PARTITION BY dea.location ORDER BY dea.location, dea.date) as RollingPeopleVaccinated
	   --,RollingPeopleVaccinated
FROM dbo.CovidDeath dea
JOIN dbo.CovidVaccination vac
    ON dea.location = vac.location
    AND dea.date = vac.date
WHERE dea.continent IS NOT NULL
--ORDER BY 2, 3

