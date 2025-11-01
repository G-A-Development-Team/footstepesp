--Created by CarterPoe
local window = gui.Window("fstep.window", "FootStep ESP by CarterPoe", 200, 200, 700, 480)
local left = gui.Groupbox(window, "Main", 16, 16, 320, 0)
local right = gui.Groupbox(window, "Filters", 360, 16, 320, 0)

local enabled = gui.Checkbox(left, "fstep.enabled", "Enable FootStep ESP", true)
local enemiesOnly = gui.Checkbox(left, "fstep.enemies", "Enemies only", false)
local life = gui.Slider(left, "fstep.life", "Footstep Lifetime (s)", 6.0, 0.5, 6.0)
local stepDist = gui.Slider(left, "fstep.stepdist", "Step Distance (u)", 39, 8, 48)
local width = gui.Slider(left, "fstep.width", "Foot Width (px)", 6, 2, 16)
local length = gui.Slider(left, "fstep.length", "Foot Length (px)", 12, 6, 28)
local alpha = gui.Slider(left, "fstep.alpha", "Max Alpha", 200, 20, 255)
local tColor = gui.ColorPicker(left, "fstep.tclr", "T Color", 230, 80, 80, 200)
local ctColor = gui.ColorPicker(left, "fstep.ctclr", "CT Color", 80, 170, 250, 200)

local audibleRun = gui.Checkbox(right, "fstep.onlyaudiblerun", "Only audible running", true)
local includeJump = gui.Checkbox(right, "fstep.includejump", "Include jump/land sounds", true)
local runSpeed = gui.Slider(right, "fstep.runspeed", "Run Speed", 180, 150, 320)
local hearingDist = gui.Slider(right, "fstep.hearing", "Hearing Distance", 1500, 100, 2000)
-- Show window when menu is open
local menu_ref = gui.Reference("menu")
callbacks.Register("Draw", function()
    if menu_ref and menu_ref:IsActive() then
        window:SetActive(true)
    else
        window:SetActive(false)
    end
end)

-- State
local playersState = {}
-- footprints: { {pos=Vector3, dir={x,y}, birth=curtime, team=2/3, side=1 or -1} }
local footprints = {}

local function clamp(v, a, b)
    if v < a then return a end
    if v > b then return b end
    return v
end

local function normalize2(x, y)
    local len = math.sqrt(x*x + y*y)
    if len == 0 then return 0, 0, 0 end
    return x/len, y/len, len
end

local function ground_at(pos)
    -- Trace 200 units down from slightly above origin to find ground
    local up = Vector3(pos.x, pos.y, pos.z + 8)
    local down = Vector3(pos.x, pos.y, pos.z - 200)
    local tr = engine.TraceLine(up, down, 0xFFFFFFFF)
    -- Derive end position via fraction (src + (dst-src)*fraction)
    local dx = down.x - up.x
    local dy = down.y - up.y
    local dz = down.z - up.z
    local frac = tr.fraction or 1.0
    local gx = up.x + dx * frac
    local gy = up.y + dy * frac
    local gz = up.z + dz * frac
    return Vector3(gx, gy, gz)
end

local function add_footprint(ent, curpos, lastpos, next_side, is_shot)
    -- Direction based on last -> current in XY
    local dx = curpos.x - lastpos.x
    local dy = curpos.y - lastpos.y
    local nx, ny, dist = normalize2(dx, dy)
    if dist < 1.0 then return end

    -- Place footprint at entity feet using mins.z, slightly raised to avoid z-fighting
    local mins = ent:GetMins()
    local gpos = Vector3(curpos.x, curpos.y, curpos.z + (mins and mins.z or -16) + 2)

    local team = ent:GetTeamNumber()
    table.insert(footprints, {
        pos = gpos,
        dirx = nx, diry = ny,
        birth = globals.CurTime(),
        team = team,
        side = next_side,
        is_shot = is_shot == true
    })
end

local function get_team_color(team)
    local r, g, b, a
    if team == 3 then
        r, g, b, a = ctColor:GetValue()
    else
        r, g, b, a = tColor:GetValue()
    end
    return r, g, b, a
end

local function draw_footprint(fp, max_alpha)
    -- Convert to screen and orient using screen-space direction
    local s0x, s0y = client.WorldToScreen(fp.pos)
    if not s0x or not s0y then return end

    local ahead = Vector3(fp.pos.x + fp.dirx * 10, fp.pos.y + fp.diry * 10, fp.pos.z + 1)
    local s1x, s1y = client.WorldToScreen(ahead)

    local footW = width:GetValue()
    local footL = length:GetValue()

    local r, g, b, _ = get_team_color(fp.team)

    -- Age based alpha
    local life_val = life:GetValue()
    local age = globals.CurTime() - fp.birth
    local t = clamp(1.0 - (age / life_val), 0.0, 1.0)
    local a = math.floor(max_alpha * t)
    if a <= 0 then return end

    draw.Color(r, g, b, a)

    if s1x and s1y then
        -- Screen-space oriented footprint: heel circle + toe triangle
        local dx = s1x - s0x
        local dy = s1y - s0y
        local ndx, ndy, dlen = normalize2(dx, dy)
        if dlen == 0 then
            draw.FilledCircle(s0x, s0y, math.floor(footW * 0.6))
            draw.OutlinedCircle(s0x, s0y, math.floor(footW * 0.6))
            return
        end
        -- Perpendicular for width
        local px = -ndy
        local py = ndx

        -- Heel circle position slightly back
        local hx = s0x - ndx * (footL * 0.25)
        local hy = s0y - ndy * (footL * 0.25)
        local heel_r = math.max(2, math.floor(footW * 0.6))
        -- Soft glow: draw multiple circles with decreasing alpha
        for i=3,1,-1 do
            local ga = math.floor(a * (0.09 * i))
            draw.Color(r, g, b, ga)
            draw.FilledCircle(hx, hy, heel_r + i)
        end
        draw.Color(r, g, b, a)
        draw.FilledCircle(hx, hy, heel_r)

        -- Toe triangle forward
        local tipx = s0x + ndx * (footL * 0.75)
        local tipy = s0y + ndy * (footL * 0.75)
        local baseLx = s0x - ndx * (footL * 0.05) + px * (footW * 0.7)
        local baseLy = s0y - ndy * (footL * 0.05) + py * (footW * 0.7)
        local baseRx = s0x - ndx * (footL * 0.05) - px * (footW * 0.7)
        local baseRy = s0y - ndy * (footL * 0.05) - py * (footW * 0.7)
        draw.Triangle(baseLx, baseLy, baseRx, baseRy, tipx, tipy)
        -- Outline accent
        draw.Color(r, g, b, clamp(a + 20, 0, 255))
        draw.OutlinedCircle(hx, hy, heel_r)
    else
        -- Fallback: simple circle
        draw.FilledCircle(s0x, s0y, math.floor(footW))
        draw.OutlinedCircle(s0x, s0y, math.floor(footW))
    end
end

local function on_draw()
    -- Custom manual-draw window removed; using gui.Window
    -- Draw custom window and handle inputs
    if false then
        local mx, my = input.GetMousePos()
        local clicked = input.IsButtonPressed(VK.MOUSE_LEFT)

        -- Dragging logic on header
        local header_h = 24
        if clicked then
            if mx >= ui.x and mx <= ui.x + ui.w and my >= ui.y and my <= ui.y + header_h then
                ui.dragging = true
                ui.drag_dx = mx - ui.x
                ui.drag_dy = my - ui.y
            end
        elseif ui.dragging and input.IsButtonDown and not input.IsButtonDown(VK.MOUSE_LEFT) then
            ui.dragging = false
        end
        if ui.dragging then
            ui.x = mx - ui.drag_dx
            ui.y = my - ui.drag_dy
        end

        -- Window drawing
        draw.SetFont(font)
        draw.Color(18, 18, 24, 200)
        draw.FilledRect(ui.x, ui.y, ui.x + ui.w, ui.y + ui.h)
        draw.Color(255, 255, 255, 12)
        draw.OutlinedRect(ui.x, ui.y, ui.x + ui.w, ui.y + ui.h)
        draw.Color(40, 40, 55, 220)
        draw.FilledRect(ui.x, ui.y, ui.x + ui.w, ui.y + header_h)
        draw.Color(255, 255, 255, 220)
        draw.Text(ui.x + 8, ui.y + 4, "FootStepESP (*)")

        -- Helper for rows
        local function row(label, value_str, y, minus_cb, plus_cb)
            draw.SetFont(small)
            draw.Color(220, 220, 230, 230)
            draw.Text(ui.x + 10, y, label .. ": " .. value_str)
            local bx = ui.x + ui.w - 64
            local by = y - 2
            local bw = 24
            local bh = 18
            -- minus
            draw.Color(60, 60, 70, 220)
            draw.FilledRect(bx, by, bx + bw, by + bh)
            draw.Color(255, 255, 255, 180)
            draw.Text(bx + 8, by + 2, "-")
            -- plus
            local bx2 = bx + bw + 8
            draw.Color(60, 60, 70, 220)
            draw.FilledRect(bx2, by, bx2 + bw, by + bh)
            draw.Color(255, 255, 255, 180)
            draw.Text(bx2 + 6, by + 2, "+")
            if clicked then
                if mx >= bx and mx <= bx + bw and my >= by and my <= by + bh and minus_cb then minus_cb() end
                if mx >= bx2 and mx <= bx2 + bw and my >= by and my <= by + bh and plus_cb then plus_cb() end
            end
        end

        -- Toggle row for enabled
        draw.SetFont(small)
        local ry = ui.y + header_h + 10
        do
            local bx = ui.x + ui.w - 28
            local by = ry - 2
            local bw = 18
            local bh = 18
            draw.Color(60, 60, 70, 220)
            draw.FilledRect(bx, by, bx + bw, by + bh)
            draw.Color(255, 255, 255, 220)
            draw.Text(ui.x + 10, ry, "Enabled")
            draw.Text(bx + 4, by + 2, settings.enabled and "✓" or " ")
            if clicked and mx >= bx and mx <= bx + bw and my >= by and my <= by + bh then
                settings.enabled = not settings.enabled
            end
        end

        -- Enemies only
        ry = ry + 24
        do
            local bx = ui.x + ui.w - 28
            local by = ry - 2
            local bw = 18
            local bh = 18
            draw.Color(60, 60, 70, 220)
            draw.FilledRect(bx, by, bx + bw, by + bh)
            draw.Color(255, 255, 255, 220)
            draw.Text(ui.x + 10, ry, "Enemies only")
            draw.Text(bx + 4, by + 2, settings.enemiesOnly and "✓" or " ")
            if clicked and mx >= bx and mx <= bx + bw and my >= by and my <= by + bh then
                settings.enemiesOnly = not settings.enemiesOnly
            end
        end

        -- Sliders via +/- buttons
        -- Dynamically extend window height based on content
        ui.h = (ry - ui.y) + 22 * 6 + 40
        ry = ry + 26
        row("Lifetime (s)", string.format("%.1f", settings.life), ry,
            function() settings.life = clamp(settings.life - 0.1, 0.5, 6.0) end,
            function() settings.life = clamp(settings.life + 0.1, 0.5, 6.0) end)

        ry = ry + 22
        row("Step Dist (u)", tostring(settings.stepDist), ry,
            function() settings.stepDist = clamp(settings.stepDist - 1, 8, 48) end,
            function() settings.stepDist = clamp(settings.stepDist + 1, 8, 48) end)

        ry = ry + 22
        row("Foot Width (px)", tostring(settings.width), ry,
            function() settings.width = clamp(settings.width - 1, 2, 16) end,
            function() settings.width = clamp(settings.width + 1, 2, 16) end)

        ry = ry + 22
        row("Foot Length (px)", tostring(settings.length), ry,
            function() settings.length = clamp(settings.length - 1, 6, 28) end,
            function() settings.length = clamp(settings.length + 1, 6, 28) end)

        ry = ry + 22
        row("Max Alpha", tostring(settings.alpha), ry,
            function() settings.alpha = clamp(settings.alpha - 5, 20, 255) end,
            function() settings.alpha = clamp(settings.alpha + 5, 20, 255) end)

        ry = ry + 22
        row("Min Walk Speed", tostring(settings.minWalkSpeed), ry,
            function() settings.minWalkSpeed = clamp(settings.minWalkSpeed - 5, 0, 300) end,
            function() settings.minWalkSpeed = clamp(settings.minWalkSpeed + 5, 0, 300) end)

        -- Audible running toggle
        ry = ry + 24
        do
            local bx = ui.x + ui.w - 28
            local by = ry - 2
            local bw = 18
            local bh = 18
            draw.Color(60, 60, 70, 220)
            draw.FilledRect(bx, by, bx + bw, by + bh)
            draw.Color(255, 255, 255, 220)
            draw.Text(ui.x + 10, ry, "Only audible running")
            draw.Text(bx + 4, by + 2, settings.onlyAudibleRunning and "✓" or " ")
            if clicked and mx >= bx and mx <= bx + bw and my >= by and my <= by + bh then
                settings.onlyAudibleRunning = not settings.onlyAudibleRunning
            end
        end

        ry = ry + 22
        row("Run Speed", tostring(settings.runSpeed), ry,
            function() settings.runSpeed = clamp(settings.runSpeed - 5, 150, 320) end,
            function() settings.runSpeed = clamp(settings.runSpeed + 5, 150, 320) end)

        ry = ry + 22
        row("Hearing Dist", tostring(settings.hearingDist), ry,
            function() settings.hearingDist = clamp(settings.hearingDist - 25, 100, 1500) end,
            function() settings.hearingDist = clamp(settings.hearingDist + 25, 100, 1500) end)
    end

    if not enabled:GetValue() then return end

    local lp = entities.GetLocalPlayer()
    if not lp then
        -- Clear all runtime state when not in a game to avoid leftover footprints on main menu
        footprints = {}
        playersState = {}
        return
    end

    local maxA = alpha:GetValue()
    local now = globals.CurTime()

    -- Iterate all player pawns
    local list = entities.FindByClass("C_CSPlayerPawn")
    for i=1, #list do
        local ent = list[i]
        if ent and ent:IsPlayer() and ent:IsAlive() and not ent:IsDormant() then
            if enemiesOnly:GetValue() and lp:IsAlive() then
                if ent:GetTeamNumber() == lp:GetTeamNumber() then
                    -- skip teammates
                    goto continue_players
                end
            end

            local idx = ent:GetIndex()
            local state = playersState[idx]
            local pos = ent:GetAbsOrigin()

            if not state then
                playersState[idx] = {
                    last = pos,
                    next_side = 1, -- 1 right, -1 left
                    acc = 0,
                }
            else
                -- Accumulate movement distance in XY
                local dx = pos.x - state.last.x
                local dy = pos.y - state.last.y
                local _, _, moved = normalize2(dx, dy)
                state.acc = state.acc + moved
                -- Place a step when threshold reached
                local threshold = stepDist:GetValue()
                if state.acc >= threshold then
                    -- Only create footprint if player is in view, visible, and not slow-walking
                    local sx, sy = client.WorldToScreen(pos)
                    if sx and sy then
                        local vel = ent.GetFieldVector and ent:GetFieldVector("m_vecVelocity") or nil
                        local speed = vel and vel:Length2D() or 0
                        local vz = vel and vel.z or 0
                        -- On ground state
                        local flags = ent.GetFieldInt and ent:GetFieldInt("m_fFlags") or 0
                        local onGround = (flags % 2) == 1
                        -- Require running speed on ground OR vertical impact/jump velocity at landing to produce audible steps
                        
                            -- In hearing distance of local player or a teammate
                            local inHearing = false
                            local lpPos = lp:GetAbsOrigin()
                            local dx = pos.x - lpPos.x
                            local dy = pos.y - lpPos.y
                            local dz = pos.z - lpPos.z
                            local dist2 = dx*dx + dy*dy + dz*dz
                            if dist2 <= (hearingDist:GetValue() * hearingDist:GetValue()) then
                                inHearing = true
                            else
                                -- Check teammates
                                local myTeam = lp:GetTeamNumber()
                                local plist = entities.FindByClass("C_CSPlayerPawn")
                                for j=1, #plist do
                                    local p2 = plist[j]
                                    if p2 and p2:IsPlayer() and p2:IsAlive() and not p2:IsDormant() and p2:GetTeamNumber() == myTeam then
                                        local p2pos = p2:GetAbsOrigin()
                                        local dx2 = pos.x - p2pos.x
                                        local dy2 = pos.y - p2pos.y
                                        local dz2 = pos.z - p2pos.z
                                        local d2 = dx2*dx2 + dy2*dy2 + dz2*dz2
                                        if d2 <= (hearingDist:GetValue() * hearingDist:GetValue()) then
                                            inHearing = true
                                            break
                                        end
                                    end
                                end
                            end
                            if audibleRun:GetValue() then okSpeed = okSpeed and inHearing end

                        if (onGround and speed >= runSpeed:GetValue()) then
                            local lpAbs = lp:GetAbsOrigin()
                            local eye = Vector3(lpAbs.x, lpAbs.y, lpAbs.z + 64)
                            local tr = engine.TraceLine(eye, pos, 0xFFFFFFFF)
                            local clear = tr and tr.fraction and tr.fraction > 0.97
                            if clear then
                                add_footprint(ent, pos, state.last, state.next_side)
                                state.next_side = -state.next_side
                                state.acc = 0
                            end
                        end
                    end
                end
                state.last = pos
                playersState[idx] = state
            end
        end
        ::continue_players::
    end

    -- Draw and prune footprints
    local life_val2 = life:GetValue()
    local out = {}
    for i=1, #footprints do
        local fp = footprints[i]
        local age = now - fp.birth
        if age < life_val2 then
            draw_footprint(fp, maxA)
            table.insert(out, fp)
        end
    end
    footprints = out
end

-- Spawn a footprint when a player shoots their gun
callbacks.Register("FireGameEvent", function(e)
    if not enabled:GetValue() then return end
    if e and e:GetName() == "weapon_fire" then
        local lp = entities.GetLocalPlayer()
        if not lp then return end
        local uid = e.GetFieldInt and e:GetFieldInt("userid") or nil
        if not uid then return end
        local ent = entities.GetByUserID(uid)
        if not ent or not ent:IsPlayer() or not ent:IsAlive() or ent:IsDormant() then return end
        if enemiesOnly:GetValue() and lp:IsAlive() and ent:GetTeamNumber() == lp:GetTeamNumber() then return end

        local pos = ent:GetAbsOrigin()
        local sx, sy = client.WorldToScreen(pos)
        if not (sx and sy) then return end
        -- LOS check
        local lpAbs = lp:GetAbsOrigin()
        local eye = Vector3(lpAbs.x, lpAbs.y, lpAbs.z + 64)
        local tr = engine.TraceLine(eye, pos, 0xFFFFFFFF)
        if not (tr and tr.fraction and tr.fraction > 0.97) then return end

        local idx = ent:GetIndex()
        local state = playersState[idx]
        if not state then
            state = { last = pos, next_side = 1, acc = 0 }
        end
        add_footprint(ent, pos, state.last or pos, state.next_side or 1)
        state.next_side = -(state.next_side or 1)
        state.last = pos
        playersState[idx] = state
    end
end)

callbacks.Register("Draw", on_draw)

print("♥ " .. GetScriptName() .. " loaded: FootStepESP active")



